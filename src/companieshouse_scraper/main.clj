(ns companieshouse-scraper.main
  (:use [clojure.http.client]
	[clojure.inspector :only (inspect inspect-tree)]
	[clojure.contrib.str-utils :only (str-join re-gsub)]
	[clojure.contrib.seq-utils :only (find-first)]
	[clojure.contrib.json.write]
	[net.cgrand.enlive-html :as html]
	[clojure.contrib.sql])
  (:require [clojure.http.resourcefully :as resourcefully]))

(def *base-url* "http://wck2.companieshouse.gov.uk/")
(def *retrys* 4)
(def *busy-sleep-time* 600000)
(def *db* nil)
(def *debug* true)

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
  (html/html-resource (make-input-stream (or (:body-seq result) result))))

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

(defn busy-page? [page]
  (= "We are currently experiencing unusually high volumes of usage and are unable to satisfy your request."
     (first (html/select page [:span.error :> text-node]))))

(defn repeat-company? [company prev-companies]
  (some #(= (:number company) (:number %)) prev-companies))

(defn add-links [numbers]
  (map #(array-map :scrape %) numbers))

(defn setup-events [term session]
  (add-links (cons 41 (repeat 42))))

(defn fucked-name? [{name :name}]
  (when (string? name)
    (re-find #"\*|\$" name)))

(defn first-good-name [prev-companies]
  (find-first #(not (fucked-name? %)) prev-companies))

(defn search-and-click
  ([link-num prev-companies]
     (search-and-click link-num prev-companies 0))
  ([link-num prev-companies num-ahead]
     (let [company (first-good-name prev-companies)]
       (when company
	 (list {:do-search (:name company)}
	       {:scrape (+ num-ahead link-num)})))))

(defn scrape-company [session link-num term prev-companies]
  (let [page (web-request (link session link-num))]
    (if (busy-page? page)
      {:events (list {:site-busy link-num})
       :prev-companies prev-companies}
      (if (not (company-page? page))
	{:events (list {:not-company-page link-num})
	 :prev-companies prev-companies}
	(let [company (scrape-details page)]
	  (if (repeat-company? company prev-companies)
	    {:events (list {:scrape (inc link-num)})
	     :prev-companies (cons company prev-companies)}
	    (let [needed-comps (if (or (> link-num 42) (fucked-name? company))
				 (cons company prev-companies)
				 (list company (first prev-companies)))]
	      (if (valid-company? company)
		{:company company :events (list {:dec-limit 1})
		 :prev-companies needed-comps}
		{:events (list {:invalid-company (:name company)})
		 :prev-companies needed-comps}))))))))

(defn special-events [prev-companies event session]
  (let [company (first prev-companies)
	last-company (second prev-companies)]
    (cond
     ; Handle first 40 companies
     (and (not company) (= (count (current-search session)) 40)
	  (not (= (:scrape event) 1)))
     (add-links (range 1 42))
     (and (:scrape event) (> (:scrape event) 80))
     (concat (list {:more-results 1}) (add-links (range 1 42)))
     (and (:scrape event) (fucked-name? company))
     (search-and-click (:scrape event) prev-companies 1))))

(defn log-error [type message]
  ; FIXME do something more useful
  (println (str "Got an error " type " with message " message)))

(defn click-more-results [session times]
  (dotimes [i times] (web-request (link session) :get {"morebut" 1})))

(defn handle-event [[event data] session prev-companies term limit f]
  (debug (str "Event: {" event ", " data "}"))
  (cond
   (= event :scrape)
   (let [result (scrape-company session data term prev-companies)]
     (when (:company result)
       (f (:company result)))
     result)
   (= event :site-busy)
   (do
     (log-error :site-busy (str "The webpage is busy, pausing for "
				*busy-sleep-time*))
     (. Thread sleep *busy-sleep-time*)
     {:events (search-and-click data prev-companies)})
   (= event :scrape-failed)
   (do (log-error :scrape-failed data)
       {:halt true})
   (= event :more-results)
   (do (click-more-results session data) nil)
   (= event :invalid-company)
   (debug (str "Skipping over invalid company: " data))
   (= event :dec-limit)
   (when limit
     (debug (str (dec limit) " scrapes left"))
     {:limit (dec limit)})
   (= event :do-search)
   {:session (new-session data) :prev-companies prev-companies}
   (= event :not-company-page)
   (do
     (log-error :invalid-company (str "Invalid company skipped after "
				      (:name (first prev-companies))))
     {:events (search-and-click data prev-companies 1)})
   :else
   (do
     (log-error :unknown-event
		(str "Unknown event " event " with data '" data "'"))
     {:halt true})))

(defn control-loop [events session prev-companies term limit f]
  (when (and (not (empty? events)) (or (not limit) (> limit 0)))
    (let [extra-events
	  (special-events prev-companies (first events) session)]
      (if extra-events
	(control-loop (concat extra-events (rest events)) session
		      prev-companies term limit f)
	(let [result (handle-event (first (seq (first events))) session
				   prev-companies term limit f)]
	  (if (:halt result)
	    (println "Told to :halt")
	    (recur (concat (:events result) (rest events))
		   (or (:session result) session)
		   (or (:prev-companies result) prev-companies)
		   (or (:term result) term)
		   (or (:limit result) limit)
		   f)))))))

(defn search
  ([term f]
     (search term nil f))
  ([term limit f]
     (let [session (new-session term)
	   events (setup-events term session)]
       (control-loop events session nil term limit f))))