(ns companieshouse-scraper.main
  (:use [clojure.http.client]
	[clojure.inspector :only (inspect inspect-tree)]
	[clojure.contrib.str-utils :only (str-join re-gsub)]
	[clojure.contrib.json.write]
	[net.cgrand.enlive-html :as html]
	[clojure.contrib.sql])
  (:require [clojure.http.resourcefully :as resourcefully]))

(def *base-url* "http://wck2.companieshouse.gov.uk/")
(def *retrys* 4)
(def *db* nil)

(defn setup-db [host port database user password]
  (def *db*
       {:classname "com.mysql.jdbc.Driver"
	:subprotocol "mysql"
	:subname (str "//" host ":" port "/" database)
	:user user
	:password password}))

(defn company-stored? [{number :number}]
  (with-query-results rs
    [(str "select number from companies where number='" number "'")]
    (doall rs)))

(defn vec-contains? [needle haystack]
  (when-not (empty? (filter #(= needle %) haystack))
    needle))

(defn save-company [company]
  (with-connection *db*
    (when-not (company-stored? company)
      (insert-values :companies
		     [:number :name :address :postcode]
		     [(:number company) (:name company)
		      (:address company) (:postcode company)])
      (doseq [[key val] company]
	(when-not (vec-contains? key [:name :number :postcode :url :address])
	  (insert-values :details
			 [:company :name :data]
			 [(:number company) (name key) val]))))))

(defn make-input-stream [input-string]
  (java.io.ByteArrayInputStream. (.getBytes (apply str input-string))))

(defn parse-html [result]
  (html/html-resource (make-input-stream (:body-seq result))))

(defn web-request
  ([url]
     (web-request url parse-html))
  ([url f]
     (web-request url f :get {}))
  ([url method params]
     (web-request url parse-html method params))
  ([url f method params]
     (f
      (if (= method :post)
	(clojure.http.client/request url method {} {} params)
	(if (not (empty? params))
	  (clojure.http.client/request (str url "?" (url-encode params)))
	  (clojure.http.client/request url))))))

(defn link
  ([session]
     (link session nil))
  ([session num]
     (str *base-url* session "/companysearch"
	  (when num (str "?link=" num)))))

(defn perform-search [term session]
  (let [params {"cnumb" "", "stype" "A", "live" "on",
		"cosearch" "1", "cname" term,
		"cosearch.x" "39", "cosearch.y" "22"}]
    (web-request (link session) :post params)
    session))

(defn new-session [term]
  (let [url (web-request *base-url* :url)
	session (second (re-find #".*\.gov\.uk/(\w+)/.*" url))]
    (perform-search term session)))

(defn postcode? [postcode]
  (and (string? postcode)
       (<= (count postcode) 10)
       (re-find #"^[A-Z]{1,2}[0-9R][0-9A-Z]? {0,1}[0-9][ABD-HJLNP-UW-Z]{2}$"
		postcode)))

(defn strip-excess-whitespace [mess]
  (when (string? mess)
    (str-join " " (re-seq #"\S+" mess))))

(defn add-comma [line]
  (let [line (strip-excess-whitespace line)]
    (if (re-find #"," line)
      (str line " ")
      (str line ", "))))

(defn strip-last [n string]
  (.substring string 0 (- (count string) n)))

(defn scrape-address [page]
  (let [addr (html/select page [:td.padding36 :> text-node])]
    (let [postcode (strip-excess-whitespace (last addr))
	  addr (apply str (map add-comma (butlast (rest addr))))]
      (when (not (empty? addr))
	(if (postcode? postcode)
	  {:address (strip-last 2 addr) :postcode postcode}
	  {:address (str addr postcode) :postcode ""})))))

(defn scrape-name-num [page]
  (let [name-num (html/select page [:td.padding36 :> :strong :> text-node])]
    (when (second name-num)
      (let [name (first name-num)
	    number (re-find #"[^ ]+$" (second name-num))]
	(when (and name number)
	  {:name name :number number})))))

(defn format-keyword [key-string]
  (keyword (.toLowerCase (re-gsub #" " "-" key-string))))

(defn scrape-details [page]
  (when page
    (let [address (scrape-address page)
	  name-num (scrape-name-num page)
	  keys (html/select page [:td.yellowCreamTable :> :strong :> text-node])
	  data (html/select page [:td.yellowCreamTable :> text-node])
	  values (map first (re-seq #":([^:])+" (strip-excess-whitespace (str-join " " data))))
	  details (map #(array-map (format-keyword %1) (re-gsub #": " "" %2)) keys values)
	  details (reduce merge details)]
      (merge name-num address details))))

(defn valid-company? [company]
  (when (and (:name company) (:number company)
	     (:address company) (> (count company) 3))
    company))

(defn company-page? [page]
  (= "Company Details"
     (first (html/select page [:td.padding36 :> :h1 :> text-node]))))

(defn repeat-company? [company prev-companies]
  (some #(= (:number company) (:number %)) prev-companies))

(defn scrape-company
  ([session link-num term prev-companies]
     (scrape-company session link-num term *retrys* prev-companies))
  ([session link-num term attempts-left prev-companies]
     (when (> attempts-left 0)
       (let [page (web-request (link session link-num))]
	 (if (not (company-page? page))
	   (scrape-company (new-session (or (:name (first prev-companies))
					    term))
			   link-num
			   prev-companies
			   term
			   (dec attempts-left))
	   (let [company (scrape-details page)]
	     (if (not (repeat-company? company prev-companies))
	       {:session session :company (valid-company? company)
		:prev-companies (if (> link-num 42)
				  (cons company prev-companies)
				  (list company))}
	       (recur session (inc link-num) term
		      (dec attempts-left)
		      (cons company prev-companies)))))))))

(defn scrape-search
  ([term f] (scrape-search term nil f))
  ([term limit f]
     (let [link-nums (cons 41 (repeat 42))
	   link-nums (if limit (take limit link-nums) link-nums)
	   session (new-session term)]
       (scrape-search term session link-nums '() f)))
  ([term session link-nums prev-companies f]
     (when (not (empty? link-nums))
       (let [result (scrape-company session
				    (first link-nums)
				    term
				    prev-companies)]
	 (when result
	   (f (:company result))
	   (recur term (:session result)
		  (rest link-nums) (:prev-companies result) f))))))

;; Todo

; Fix first 40 issue
;  - when count of search results is less than 40 replace
;    (repeat 42) with (cons (range count) (repeat 42))

;; Edge cases
; Search for "CHEERSTAGE PROPERTIES LTD." 6/7 results in - repeat
; Search for "A & J LTD" 3? results in - loop
; "A & J FRENCH (OXFORD) LIMITED" - No address or details!