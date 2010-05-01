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
(def *first-company* "07165598")
(def *debug* nil)

(defn debug [msg]
  (when *debug*
    (println msg)))

(defn setup-db [host port database user password]
  (def *db*
       {:classname "com.mysql.jdbc.Driver"
	:subprotocol "mysql"
	:subname (str "//" host ":" port "/" database)
	:user user
	:password password}))

(defn company-stored? [{number :number}]
  (with-query-results rs
    ["select number from companies where number=?" number]
    (doall rs)))

(defn vec-contains? [needle haystack]
  (when-not (empty? (filter #(= needle %) haystack))
    needle))

(defn save-company [company]
  (debug (str "Saving company: " (:name company)))
  (with-connection *db*
    (when-not (company-stored? company)
      (insert-values :companies
		     [:number :name :address :postcode :details]
		     [(:number company) (:name company)
		      (:address company) (:postcode company)
		      (:details company)]))))

(defn make-input-stream [input-string]
  (java.io.ByteArrayInputStream. (.getBytes (apply str input-string))))

(defn parse-html [result]
  (html/html-resource (make-input-stream (:body-seq result))))

(defn format-url [url method params]
  (if (and (= (method :get)) (not (empty? params)))
    (str url "?" (url-encode params))
    url))

(defn web-request
  ([url]
     (web-request url parse-html))
  ([url f]
     (web-request url f :get {}))
  ([url method params]
     (web-request url parse-html method params))
  ([url f method params]
     (f
      (try
       (clojure.http.client/request (format-url url method params)
				    method {} {}
				    (when (= method :post) params))
       (catch java.net.ConnectException _ nil)))))

(defn link
  ([session]
     (link session nil))
  ([session num]
     (str *base-url* session "/companysearch"
	  (when num (str "?link=" num)))))

(defn perform-search
  ([term session]
     (perform-search term session nil))
  ([term session f]
     (let [params {"cnumb" "", "stype" "A", "live" "on",
		   "cosearch" "1", "cname" term,
		   "cosearch.x" "39", "cosearch.y" "22"}
	   response (web-request (link session) :post params)]
       (if f
	 (f response)
	 session))))

(defn new-session [term]
  (let [url (web-request *base-url* :url)
	session (second (re-find #".*\.gov\.uk/(\w+)/.*" url))]
    (perform-search term session)))

(defn current-search [session]
  (let [link-selector [:table.compResultTable
		       (attr-contains :href "companysearch?link=")]
	source (web-request (link session))
	links (map #(Integer/parseInt (re-find #"[0-9]+$" (:href (:attrs %))))
		   (html/select source link-selector))]
    links))

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

(defn scrape-details [page]
  (when page
    (let [address (scrape-address page)
	  name-num (scrape-name-num page)
	  details (apply str (html/emit* (html/select page [:td.yellowCreamTable])))]
      (merge name-num address {:details details}))))

(defn valid-company? [company]
  (when (and (not (empty? (:name company)))
	     (not (empty? (:number company)))
	     (string? (:address company))
	     (string? (:details company)))
    company))

(defn company-page? [page]
  (= "Company Details"
     (first (html/select page [:td.padding36 :> :h1 :> text-node]))))

(defn repeat-company? [company prev-companies]
  (some #(= (:number company) (:number %)) prev-companies))

(defn add-links [numbers]
  (map #(array-map :scrape %) numbers))

(defn special-events [company last-company event session]
  (cond
   ; Handle first 40 companies
   (and (not company) (= (count (current-search session)) 40)
	(not (= (:scrape event) 1)))
   (add-links (range 1 41))
   ; Handle companies that jump results back to start
   (and (not (empty? last-company))
	(= (:number company) *first-company*)
	(:scrape event))
   (list {:do-search (:name last-company)}
	 {:scrape (inc (:scrape event))})))

(defn setup-events [term session]
  (add-links (cons 41 (repeat 42))))

(defn scrape-company [session link-num term attempts-left prev-companies]
  (if (< attempts-left 1)
    {:events (list {:scrape-failed prev-companies}) :session session}
    (let [page (web-request (link session link-num))]
      (if (not (company-page? page))
	(scrape-company (new-session (or (:name (first prev-companies))
					 term))
			link-num
			term
			(dec attempts-left)
			prev-companies)
	(let [company (scrape-details page)]
	  (if (not (repeat-company? company prev-companies))
	    {:session session :company (valid-company? company)
	     :prev-companies (if (> link-num 42)
			       (cons company prev-companies)
			       (list company))}
	    (when (< link-num 80)
	      (recur session (inc link-num) term
		     attempts-left
		     (cons company prev-companies)))))))))

(defn log-error [type message]
  ; FIXME do something more useful
  (println (str "Got an error " type " with message " message)))

(defn handle-event [[event data] session prev-companies term f]
  (debug (str "Event: {" event ", " data "}"))
  (cond
   (= event :scrape)
   (let [result (scrape-company session data term *retrys* prev-companies)]
     (when (:company result)
       (f (:company result)))
     result)
   (= event :scrape-failed)
   (log-error :scrape-failed data)
   (= event :more-results)
   (do (click-more-results session data) nil)
   (= event :do-search)
   (do (perform-search data session) nil)))

(defn control-loop [events session prev-companies term limit f]
  (when (and (not (empty? events)) (or (not limit) (> limit 0)))
    (let [special-events (special-events (first prev-companies)
					 (second prev-companies)
					 (first events) session)]
      (if special-events
	(control-loop (concat special-events events) session
		      prev-companies term limit f)
	(let [result (handle-event (first (seq (first events))) session
				   prev-companies term f)
	      limit (when limit (dec limit))]
	  (recur (rest events) (or (:session result) session)
		 (:prev-companies result)
		 (:or (:term result) term) limit f))))))

(defn search
  ([term f]
     (search term nil f))
  ([term limit f]
     (let [session (new-session term)
	   events (setup-events term session)]
       (control-loop events session nil term limit f))))