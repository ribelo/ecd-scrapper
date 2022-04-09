(ns ecd-scrapper.core
  (:require
   [missionary.core :as mi]
   [taoensso.timbre :as timbre]
   [ribelo.fatum :as f]
   [cljs-bean.core :refer [->js ->clj]]
   ["date-fns" :as dtf]
   ["playwright-chromium" :as playwright]
   ["process" :as process]
   ["googleapis" :as googleapis]
   ["express" :as express]
   ["axios" :as axios]))

(def google (.-google ^js googleapis))

(def auth
  (let [<v (mi/dfv)]
    (-> (.. google -auth (getClient #js {:scopes #js ["https://www.googleapis.com/auth/drive"
                                                      "https://www.googleapis.com/auth/drive.file"
                                                      "https://www.googleapis.com/auth/drive.metadata"
                                                      "https://www.googleapis.com/auth/spreadsheets"]}))
        (.then (fn [^js obj] (<v obj)))
        (.catch (fn [^js err] (<v err))))
    <v))
(def sheets (mi/sp (.sheets google (->js {:version :v4 :auth (mi/? auth)}))))

(def drive (mi/sp (.drive google (->js {:version :v3 :auth (mi/? auth)}))))

(defn do! [sp]
  (sp #() #(js/console.error %)))

(defn tap-task [sp]
  (sp #(tap> [:ok %]) #(tap> [:err % (ex-message %)])))

(defn <p [^js p]
  (let [<v (mi/dfv)]
    (-> p (.then (fn [^js resp] (<v resp))) (.catch (fn [^js err] (<v f/ensure-fail err))))
    <v))

(defn run-browser [& {:keys [headless?]}]
  (let [<v (mi/dfv)]
    (-> (.launch (.-chromium playwright) #js {:headless (boolean headless?)})
        (.then (fn [^js obj] (<v (fn [] obj))))
        (.catch (fn [^js err] (<v (fn [] (f/fail! (f/ensure-fail err)))))))
    (mi/absolve <v)))

(defn new-context [^js browser]
  (<p (.newContext browser)))

(defn create-page [browser]
  (let [<v (mi/dfv)]
    (-> (.newPage ^js browser)
        (.then (fn [^js obj] (<v (fn [] obj))))
        (.catch (fn [^js err] (<v (fn [] (f/fail! (f/ensure-fail err)))))))
    (mi/absolve <v)))

(defn goto [page url]
  (<p (.goto ^js page url)))

(defn locator [page selector]
  (.locator ^js page selector))

(defn fill [locator value]
  (<p (.fill ^js locator value)))

(defn click [locator]
  (<p (.click ^js locator)))

(defn content [page]
  (<p (.content ^js page)))

(defn wait-for-navigation [page]
  (<p (.waitForNavigation ^js page)))

(defn wait-for-response [page url]
  (<p (.waitForResponse ^js page url)))

(defn get-token-from-localstorage [^js context]
  (<p (-> (.storageState context)
          (.then (fn [^js ok] (reduce
                           (fn [_ ^js obj]
                             (when (= "access_token" (.-name obj)) (reduced (.-value obj))))
                           (some-> ok .-origins (nth 0) .-localStorage))))
          (.catch (fn [^js err] (f/ensure-fail err))))))

(defn login-to-ecd []
  (mi/sp
   (let [username (.. process -env -ECD_USERNAME)
         password (.. process -env -ECD_PASSWORD)]
     (if (and username password)
       (let [browser (mi/? (run-browser))
             context (mi/? (new-context browser))
             page (mi/? (create-page context))]
         (mi/? (goto page "https://www.eurocash.pl"))
         (mi/? (click (locator page "#c-p-bn")))
         (mi/? (click (locator page "#ecHeader > div.fi.relative > div.menu.menu--desktop > div > a.btn.btn--green-login.m-r-25")))
         (mi/? (fill (locator page "#login") username))
         (mi/? (fill (locator page "#password") password))
         (mi/? (mi/join vector
                        (wait-for-navigation page)
                        (click (locator page "#submit_button"))))
         (let [id (mi/? (get-token-from-localstorage context))]
           (.close ^js browser)
           id))
       (f/fail "no username or password" {:username username :password password})))))

(defn create-headers [token]
  {"Authorization" (str "Bearer " token)
   "Business-Unit" "ECT"
   "Accept" "application/json, text/plain, */*"
   "Accept-Encoding" "gzip, deflate, br"
   "Content-Type" "application/json"})

(defn parse-items [^js item]
  (when-let [ean (.. item -KodKreskowy)]
    (let [name (.. item -TowarNazwa)
          net-price (.. item -CenaNettoRealizacja)
          gross-price (.. item -CenaBruttoRealizacja)]
      {:name name
       :ean ean
       :net-price net-price
       :gross-price gross-price})))

(defn parse-products-elem [^js data]
  (let [tc (.. data -Data -TotalCount)]
    {:total-count tc
     :items (mapv parse-items (.. data -Data -Items))}))

(defn query-products
  [{:keys [token query skip size]
    :or {query "" skip 0 size 30}
    :as opts}]
  (let [<v (mi/dfv)]
    (-> (axios "https://ehurtapi.eurocash.pl/api/offer/getOfferListWithPromotions"
               (->js {:method :post
                      :headers (create-headers token)
                      :data {:Filter {:Search query}
                             :PaginationOptions {:Skip skip :Size size}}}))
        (.then (fn [^js resp]
                 (if (.-Success (.-data resp))
                   (<v (parse-products-elem (.-data resp)))
                   (<v (f/fail (.-Message resp) {:type :err})))))
        (.catch (fn [err] (<v (f/fail (ex-message err))))))
    <v))

(defn fetch-products
  [{:keys [token query skip size total-count try-count]
    :or {query "" skip 0 size 30 try-count 0}
    :as opts}]
  (mi/sp
   (when (zero? skip)
     (timbre/info "token" token))
   (if token
     (when (or (not total-count) (< skip total-count))
       (f/if-ok [data (mi/? (query-products opts))]
         (let [n (:total-count data)
               items (:items data)]
           (timbre/infof "continue query: %s size: %d skip: %d :total-count %d" query size skip total-count)
           (when (seq items)
             (into items (filter identity) (mi/? (fetch-products (assoc opts :skip (+ skip size) :size size :total-count n))))))
         (when (< try-count 10)
           (timbre/warn "try again")
           (timbre/warn (ex-message data))
           (mi/? (fetch-products (update opts :try-count inc))))))
     (f/fail "invalid token" {:token token}))))

(defn create-spreadsheet [title]
  (mi/sp
   (let [<v (mi/dfv)
         ^js sheets (mi/? sheets)]
     (-> (.. sheets -spreadsheets
             (create
              (->js {:requestBody {:properties {:title title}
                                   :sheets [{:properties {:title "offer"}}]}})))
         (.then (fn [^js obj]
                  (if (<= 200 (.-status obj) 299)
                    (<v (.. obj -data -spreadsheetId))
                    (<v (f/fail (.-statusText obj) {:title title})))))
         (.catch (fn [^js err] (<v (f/ensure-fail err)))))
     (mi/? <v))))

(defn append-data [id sheet-name data]
  (mi/sp
   (let [<v (mi/dfv)
         ^js sheets (mi/? sheets)]
     (-> (.. sheets -spreadsheets -values
             (append
              (->js {:spreadsheetId id
                     :range sheet-name
                     :valueInputOption "USER_ENTERED"
                     :requestBody {:values data}})))
         (.then (fn [^js obj] (<v obj)))
         (.catch (fn [^js err] (<v (f/ensure-fail err)))))
     (mi/? <v))))

(defn share-file [id email]
  (mi/sp
   (let [<v (mi/dfv)
         ^js drive (mi/? drive)]
     (-> (.. drive -permissions
             (create
              (->js {:fileId id
                     :permissionId ""
                     :requestBody {:role :writer
                                   :type :user
                                   :emailAddress email}})))
         (.then (fn [^js obj] (<v obj)))
         (.catch (fn [^js err] (<v (f/ensure-fail err)))))
     (mi/? <v))))

(defn list-all-files []
  (mi/sp
   (let [<v (mi/dfv)
         ^js drive (mi/? drive)]
     (-> (.. drive -files
             (list
              (->js {:corpora "user"
                     :q "name contains 'ecd-offer'"})))
         (.then (fn [^js obj] (<v (mapv (fn [^js o] (.-id o)) (.. obj -data -files)))))
         (.catch (fn [^js err] (<v (f/ensure-fail err)))))
     (mi/? <v))))

(defn delete-file [file-id]
  (mi/sp
   (let [<v (mi/dfv)
         ^js drive (mi/? drive)]
     (-> (.. drive -files
             (delete
              (->js {:fileId file-id})))
         (.then (fn [^js obj] (<v obj)))
         (.catch (fn [^js err] (<v (f/ensure-fail err)))))
     (mi/? <v))))

(defn delete-all-files []
  (mi/sp
   (doseq [id (mi/? (list-all-files))]
     (mi/? (delete-file id)))))

(defn today-title []
  (str "ecd-offer_" (dtf/format (js/Date.) "yyyy-MM-dd")))

(defn create-ecd-offer-spreadsheet [data]
  (mi/sp
   (f/when-ok [id (mi/? (create-spreadsheet (today-title)))]
     (timbre/info "successful creating spreadsheet")
     (let [values (reduce
                   (fn [acc {:keys [name ean net-price gross-price]}]
                     (conj acc [name ean net-price gross-price]))
                   [["name" "ean" "net-price" "gross-price"]]
                   data)]
       (f/when-ok [resp (mi/? (append-data id "offer" values))]
         (timbre/info "successfull append data"))
       (f/when-ok [resp (mi/? (share-file id "r.krzywaznia@teas.com.pl"))]
         (timbre/info "successfull sharing spreadsheet"))))))

(defn main [& args]
  (let [port (or (.. process -env -PORT) 8080)
        ^js app (express)]
    (.get app "/"
          (fn [_req ^js res]
            ((mi/sp
              (timbre/info "handle request")
              (f/when-ok! [token (mi/? (login-to-ecd))
                           products (mi/? (fetch-products {:token token}))]
                (f/when-ok! (mi/? (create-ecd-offer-spreadsheet products))
                 (.send res "success"))))
             (fn [_] (timbre/info :success))
             (fn [err] (timbre/error err)))))
    (.get app "/delete-all"
          (fn [_req ^js res]
            ((mi/sp
              (mi/? (delete-all-files))
              (.send res "success"))
             (fn [_] (timbre/info :success))
             (fn [err] (timbre/error err)))))
    (.listen app port)))
