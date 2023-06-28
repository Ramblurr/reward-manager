(ns app.reward
  (:gen-class)
  (:require
   [clojure.tools.logging :as log]
   [ring.util.response :as rr]
   [ring.middleware.content-type :as rmc]
   [hiccup2.core :as hiccup]
   [clojure.java.io :as io]
   [org.httpkit.server :as srv]
   [cognitect.transit :as transit]
   [medley.core :as m]
   [clojure.walk :as walk]
   [clojure.core.match.regex]
   [clojure.core.match :refer [match]]
   [clojure.string :as str])
  (:import [java.net URLDecoder]
           [java.io ByteArrayOutputStream FileInputStream]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(defn safe-parse-long [v]
  (when v
    (Long/parseLong v)))

(def host (or (System/getenv "REWARD_HOST") "http://localhost"))
(def port (or (safe-parse-long (System/getenv "REWARD_PORT")) 8081))
(def data-file (or (System/getenv "REWARD_DATA") "data.json"))
(defonce server (atom nil))
(defonce state (atom nil))

;;; DATA

(def package-names {"All Merch" "All Merch (Cup, Poster, T-Shirt, Exklusives Tie-Dye-T-Shirt)"
                    "Backstage" "Backstage Access + All Merch (Cup, Poster, T-Shirt, Exklusives Tie-Dye-T-Shirt)"
                    "Tie-Dye" "Exklusives Tie-Dye-T-Shirt"
                    "T-Shirt" "Festival T-Shirt"
                    "Cup+Poster" "Strafiato Becher+Poster"
                    "Poster" "Strafiato Poster"
                    "none" "Ohne Geschenk unterstützen "})

(def sizes [{:value "xs" :label "XS"}
            {:value "s" :label "S"}
            {:value "m" :label "M"}
            {:value "l" :label "L"}
            {:value "xl" :label "XL"}])

(def colors [{:value "orange-on-maroon" :label "Orange auf Rot"}
             {:value "black-on-maroon" :label "Schwarz auf Rot"}
             {:value "black-on-yellow" :label "Schwarz auf Gelb"}
             {:value "orange-on-yellow" :label "Orange auf Gelb"}])

;;; STATE

(defn save-state! [state data-file]
  (when @state
    (let [out (ByteArrayOutputStream. 1000000)
          writer (transit/writer out :json)]
      (transit/write writer @state)
      (spit (io/file data-file) (.toString out)))))

(defn load-state! [state data-file]
  (reset! state (transit/read (transit/reader (FileInputStream. data-file) :json))))

(defn current-date-time []
  (let [formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd-HH-mm-ss")
        now (LocalDateTime/now)]
    (-> formatter (.format now))))

(defn backup-state! [data-file]
  (let [time-str (current-date-time)
        dest-path (str data-file ".backup." time-str)]
    (io/copy (io/file data-file) (io/file dest-path))))

(comment
  (load-state! state data-file)
  @state
  (save-state! state data-file)

  (sort (into [] (keys  (:codes  @state))))

  ;;
  )

(defn get-by-code [code]
  (let [code (str/upper-case code)
        email (get-in @state [:codes code])]
    (if email
      (get-in @state [:supporters email])
      :nope)))

(defn update-supporter! [supporter]
  (swap! state (fn [state]
                 (assoc-in state [:supporters (:email supporter)] supporter)))

  (save-state! state data-file)
  supporter)

;;; HTTP HELPERS
;;;  FROM CTMX

(defn prune-params [m]
  (if (= 1 (count m))
    (cond
      (and (vector? m) (-> m peek coll?))
      (-> m peek recur)
      (and (map? m) (-> m first second coll?))
      (-> m first second recur)
      :else m)
    m))

(defn nest-params [params]
  (reduce
   (fn [m [k v]]
     (assoc-in m (-> k name (.split "_")) v))
   {}
   params))

(defn- digital? [[k]]
  (boolean (re-find #"^\d+$" k)))
(defn- key-value [[k]]
  (Long/parseLong k))
(defn- conjv [s x]
  (conj (or s []) x))

(defn vectorize-map [m]
  (if (map? m)
    (let [{digital-elements true normal-elements false} (group-by digital? m)
          normal-map (into {} normal-elements)
          digital-sorted (->> digital-elements (sort-by key-value) (map second))]
      (reduce
       (fn [m sorted-item]
         (reduce
          (fn [m [k v]]
            (update m k conjv v))
          m
          sorted-item))
       normal-map
       digital-sorted))
    m))

(defn json-params [params]
  (->> params
       nest-params
       (walk/postwalk vectorize-map)
       walk/keywordize-keys))

(defn json-params-pruned [params]
  (-> params json-params prune-params))

;;;;; END FROM CTMX

(defn parse-body [body]
  (-> body
      slurp
      (str/split #"=")
      second
      URLDecoder/decode))

(defn parse-form-params [params-str]
  (let [pairs (str/split params-str #"&")]
    (reduce (fn [result pair]
              (let [[key value] (str/split pair #"=")]
                (assoc result key value)))
            {}
            pairs)))
(defn parse-params [body]
  (-> body
      slurp
      parse-form-params
      json-params-pruned))
;; END HTML HELPERS

(defn head [title]
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport"
           :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
   [:title (or title "Strafiato Rewards")]
   [:link {:rel "stylesheet" :href "/css/simple.min.css"}]
   [:script {:src "/js/htmx.org@1.9.2.js"}]])

(defn cache-control [resp]
  (if (= 200 (:status resp))
    (assoc-in resp [:headers "cache-control"] (str "max-age=604800"))
    resp))

(defn not-found
  []
  {:status  404
   :headers {}
   :body    ""})

(defn asset-response
  [req [asset-type filename]]
  (if-let [resp (rr/resource-response (str asset-type "/" filename) {:root "public"})]
    (-> resp
        (rmc/content-type-response req)
        (cache-control))
    (not-found)))

(defn html5-response
  [body]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str (hiccup/html
               (head nil)
               [:body body]))})
(defn goodbye []
  (list
   [:p "Mit groovigen Grüßen,"]
   [:p "StreetNoise Orchestra"]))

(defn pickup-info [code]
  (list
   [:p "Denk daran, deinen Unterstützercode mitzubringen!"]
   [:p [:strong [:span {:style "font-size: 3rem;"} code]]]
   [:p "Dein Dankeschöns kannst du am 6.-8. Juli ab 17:00 Uhr am Merch-Stand im Treibhaus abholen. "]
   [:p
    "Falls du noch Fragen hast, schreibe uns gerne eine Mail: " [:a {:href "mailto:orchestra@streetnoise.at"} "orchestra@streetnoise.at"] ". Wir sind für dich da!"]
   [:p]
   (goodbye)))

(defn code-form []
  [:form {:hx-post "/rewards" :hx-target "#content" :hx-push-url "true"}
   [:p
    [:label "Unterstützercode"]
    [:input {:type "text" :name "code" :value "" #_"A-GKS3A" :style "text-transform:uppercase;"}]]
   [:button "Weiter"]])

(defn page-home []
  [:div
   [:h2 "Strafiato Belohnungsmanager"]
   [:div {:id "content"}
    [:p "Dein Tor zu exklusiven Strafiato-Dankeschöns: Verwalte und Personalisiere deine Belohnungen hier!"]
    (code-form)]])

(defn form-size [name selected-value]
  (let [name (str name "_size")]
    [:div
     [:p
      [:label [:strong  "Größe"]]

      (map (fn [{:keys [value label]}]
             [:label
              [:input {:checked (= selected-value value) :name name :type "radio" :value value}] label]) sizes)]
     [:details
      [:summary "Größeninformationen"]
      [:p
       [:a {:target "_blank" :href "img/sizing.png"} [:img {:src "img/sizing.png"}]]
       [:a {:target "_blank" :href "img/size-chart.png"} [:img {:src "img/size-chart.png"}]]]]]))

(defn form-color [name selected-value]
  (let [name (str name "_color")]
    [:p
     [:label [:strong "Farbe"]]
     (map (fn [{:keys [value label]}]
            [:label
             [:input {:checked (= selected-value value) :name name :type "radio" :value value}] label]) colors)]))

(defn form-festival-shirt [idx size color]
  (let [name (str "choice_" idx "_shirt")]
    (list
     (form-size name size)
     (form-color name color))))

(defn form-tie-dye-shirt [idx size]
  (let [name (str "choice_" idx "_tiedye")]
    (list
     (form-size name size))))

(defn comp-reward-choice [idx {:keys [type size color]}]
  (list
   [:h3 (str (inc idx) ". ") (get {:tie-dye "Exklusives Tie-Dye-T-Shirt" :tshirt "Festival T-Shirt"} type)]
   [:picture
    (let [src (get {:tie-dye "img/tie-dye.jpg" :tshirt "img/combo.jpg"} type)
          src-wide (get {:tie-dye "img/tie-dye.jpg" :tshirt "img/combo-wide.jpg"} type)]
      (list
       [:source {:srcset src :media "(max-width: 800px)"}]
       [:source {:srcset src-wide  :media "(min-width: 801px)"}]
       [:img {:src src :decoding :auto :loading :eager :fetchpriority :high :alt "Strafiato Festival T-Shirt Options"}]))]
   (condp = type
     :tshirt (form-festival-shirt idx size color)
     :tie-dye (form-tie-dye-shirt idx size))))

(def intro-text
  (list
   [:p "Vielen Dank für deine grandiose Unterstützung bei der Crowdfunding-Kampagne für unser Urban Brass Festival, Strafiato!"]
   [:p "Dank dir wird Innsbruck vom 6. bis 9. Juli durch die Energie und Leidenschaft der Brassbands belebt."]))

(defn page-rewards [{:keys [body]}]
  (let [code (str/upper-case (parse-body body))
        {:keys [email donations user-choices name]} (get-by-code code)]
    (tap> {:d donations})
    (if email
      [:div
       [:p (str "Hallo " name "!")]
       intro-text
       [:section
        [:h3 "Deine Spenden"]
        [:ul
         (map (fn [{:keys [package] :as d}]
                [:li
                 [:strong (get package-names package)]  [:br] (:Date d)  " €" (:Amount d)]) donations)]]
       (if (seq user-choices)
         [:section
          [:h3 "Bitte auswählen"]
          [:form {:hx-post "/rewards-confirm" :hx-target "#content" :hx-push-url "true"}
           [:input {:type :hidden :name "code" :value code}]
           (map-indexed comp-reward-choice user-choices)
           [:button "Zusagen"]]]
         [:div
          (goodbye)])]

      [:div
       [:p "Hoppla! Es sieht so aus, als wäre der eingegebene Unterstützercode nicht vorhanden. Bitte überprüfe deinen Code und versuche es erneut. Du solltest deinen Unterstützercode per E-Mail von uns erhalten haben."]
       (code-form)
       [:p "Falls du weiterhin Schwierigkeiten hast oder deinen Code nicht finden kannst, sende uns bitte eine E-Mail an: "
        [:a {:href "mailto:orchestra@streetnoise.at"} "orchestra@streetnoise.at"]
        ". Wir helfen dir gerne weiter!"]
       [:p "Mit groovigen Grüßen,"]
       [:p "StreetNoise Orchestra"]])))

(defn merge-vectors [vec1 vec2]
  (map-indexed (fn [idx map2]
                 (let [map1 (get vec1 idx)]
                   (if map1
                     (merge map2 map1)
                     map2)))
               vec2))

(def choice-keys [:type :size :color])
;; (flatten '([{:idx "0", :size "XS"}] [{:idx "1", :size "S", :color "Yellow"}]))
(defn page-rewards-confirm [{:keys [body]}]
  (let [params (parse-params body)
        choices
        (mapv
         (fn [data]
           (select-keys
            (m/map-kv (fn [k v]
                        (if (= k :idx)
                          [k (Long/parseLong v)]
                          [k v])) data) choice-keys))
         (flatten (vals (:choice params))))
        code  (:code params)
        supporter (get-by-code code)
        user-choices (:user-choices supporter)
        new-user-choices (merge-vectors choices user-choices)]
    (update-supporter! (assoc supporter :user-choices new-user-choices))

    [:div
     [:p "Erinner daran, du kannst deine Auswahl jederzeit bis zum 5. Juli unter "
      [:a {:href "https://rewards.streetnoise.at"} "https://rewards.streetnoise.at"] " ändern."]
     (pickup-info code)]))

(defn log-request
  [start-time req resp]
  (let [{:keys [uri request-method query-string]} req
        finish (System/currentTimeMillis)
        total (- finish start-time)]
    (log/info :msg "request completed"
              :method (str/upper-case (name request-method))
              :uri uri
              :query-string query-string
              :status (:status resp)
              :response-time total)))

(defn app [{:keys [:request-method :uri] :as req}]
  (let [start-time (System/currentTimeMillis)
        path (vec (rest (str/split uri #"/")))
        resp (match [request-method path]
               [:get []] (html5-response (page-home))
               [:get ["img" #".*\.(jpg|png)"]] (asset-response req path)
               [:get ["js" #".*\.js"]] (asset-response req path)
               [:get ["css" #".*\.css"]] (asset-response req path)
               [:post ["rewards"]] (html5-response (page-rewards req))
               [:post ["rewards-confirm"]] (html5-response (page-rewards-confirm req))
               :else {:status 404 :body "Error 404: Page not found"})]
    (log-request start-time req resp)
    resp))

(defn go []
  (backup-state! data-file)
  (load-state! state data-file)
  (reset! server
          (srv/run-server app {:port port}))
  (println "serving" (str host ":" port "/")))

(defn stop []
  (save-state! state data-file)
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))

(defn restart []
  (stop)
  (go))

(defn -main [& args]
  (go)
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread.
    (fn []
      (stop))))

  @(promise))

(comment
  (go)
  (stop)
  (restart) ;; rcf

  (get-by-code "G-6E9WF")
  ;;
  )
