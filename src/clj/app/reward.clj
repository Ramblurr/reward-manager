(ns app.reward
  (:require
   [hiccup2.core :as hiccup]
   [clojure.java.io :as io]
   [org.httpkit.server :as srv]
   [cognitect.transit :as transit]
   [medley.core :as m]
   [clojure.walk :as walk]
   [clojure.core.match :refer [match]]
   [clojure.string :as str])
  (:import [java.net URLDecoder]
           [java.io ByteArrayOutputStream FileInputStream]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(def host (or (System/getenv "REWARD_HOST") "http://localhost"))
(def port (or (System/getenv "REWARD_PORT") 8081))
(def data-file (or (System/getenv "REWARD_DATA") "data.json"))
(defonce server (atom nil))
(defonce state (atom nil))

;;; DATA

(def package-names {"All Merch" "All Merch (Cup, Poster, T-Shirt, Exklusives Tie-Dye-T-Shirt)"
                    "Backstage" "Backstage Access + All Merch (Cup, Poster, T-Shirt, Exklusives Tie-Dye-T-Shirt)"
                    "Tie-Dye" "Exklusives Tie-Dye-T-Shirt"
                    "T-Shirt" "Festival T-Shirt"
                    "Cup+Poster" "Strafiato Becher+Poster"
                    "Poster" "Strafiato Poster"})

(def sizes [{:value "xs" :label "XS"}
            {:value "s" :label "S"}
            {:value "m" :label "M"}
            {:value "l" :label "L"}
            {:value "xl" :label "XL"}])

(def colors [{:value "yellow" :label "Yellow"}
             {:value "red" :label "Red"}])

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

  ;;
  )

(defn get-by-code [code]
  (let [email (get-in @state [:codes code])]
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
   [:link {:rel "stylesheet" :href "https://cdn.simplecss.org/simple.min.css"}]
   [:script {:src "https://unpkg.com/htmx.org@1.9.2"}]])

(defn html5-response
  [body]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str (hiccup/html
               (head nil)
               [:body body]))})

(defn pickup-info [code]
  (list
   [:p "Denk daran, deinen Unterstützercode mitzubringen!"]
   [:p [:strong [:span {:style "font-size: 3rem;"} code]]]
   [:p "Dein cooles Merch kannst du am 7. oder 9. Juli nach 17:00 Uhr am Merch-Stand im Treibhaus abholen. "]
   [:p "Falls du Fragen hast, zögere nicht, uns eine Mail zu schicken: " [:a {:href "mailto:orchestra@streetnoise.at"} "orchestra@streetnoise.at"] ". Wir sind für dich da!"]
   [:p]
   [:p "Mit groovigen Grüßen,"]
   [:p "StreetNoise Orchestra"]))

(defn page-home []
  [:div
   [:h2 "Strafiato Belohnungsmanager"]
   [:div {:id "content"}
    [:p "Dein Tor zu exklusiven Strafiato-Dankeschöns: Verwalte und Personalisiere deine Belohnungen hier!"]
    [:form {:hx-post "/rewards" :hx-target "#content"}
     [:p
      [:label "Unterstützercode"]
      [:input {:type "text" :name "code" :value ""}]]
     [:button "Continue"]]]])

(defn form-size [name selected-value]
  (let [name (str name "_size")]
    [:p
     [:label "Size"]
     (map (fn [{:keys [value label]}]
            [:label
             [:input {:checked (= selected-value value) :name name :type "radio" :value value}] label]) sizes)]))

(defn form-color [name selected-value]
  (let [name (str name "_color")]
    [:p
     [:label "Color"]
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
   [:h4 (get {:tie-dye "Exklusives Tie-Dye-T-Shirt" :tshirt "Festival T-Shirt"} type)]
   (condp = type
     :tshirt (form-festival-shirt idx size color)
     :tie-dye (form-tie-dye-shirt idx size))))

(def intro-text
  (list
   [:p "Vielen Dank für deine grandiose Unterstützung bei der Crowdfunding-Kampagne für unser Urban Brass Festival, Strafiato!"]
   [:p "Dank dir wird Innsbruck vom 6. bis 9. Juli durch die Energie und Leidenschaft der Brassbands belebt."]))

(defn page-rewards [{:keys [body]}]
  (let [code (parse-body body)
        {:keys [has-rewards? email donations user-choices name]} (get-by-code code)]
    (if email
      [:div
       [:p (str "Hallo " name "!")]
       intro-text
       (when has-rewards?
         (list
          [:section
           [:h3 "Deine Belohnungen"]
           [:ul
            (map (fn [{:keys [package] :as d}]
                   [:li
                    (get package-names package)  [:br]  "€" (:Amount d) " - " (:Date d)]) donations)]]
          (if (seq user-choices)
            [:section
             [:h3 "Bitte auswählen"]
             [:form {:hx-post "/rewards-confirm" :hx-target "#content"}
              [:input {:type :hidden :name "code" :value code}]
              (map-indexed comp-reward-choice user-choices)
              [:button "Confirm"]]]
            [:div
             (pickup-info code)])))]

      [:div
       [:p "Hoppla! Es sieht so aus, als wäre der eingegebene Unterstützercode nicht vorhanden. Bitte überprüfe deinen Code und versuche es erneut. Du solltest deinen Unterstützercode in deiner E-Mail finden können."]
       [:p "Falls du weiterhin Schwierigkeiten hast oder deinen Code nicht finden kannst, sende uns bitte eine E-Mail an"
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

(defn app [{:keys [:request-method :uri] :as req}]
  (let [path (vec (rest (str/split uri #"/")))]
    (match [request-method path]
      [:get []] (html5-response (page-home))
      [:post ["rewards"]] (html5-response (page-rewards req))
      [:post ["rewards-confirm"]] (html5-response (page-rewards-confirm req))
      :else {:status 404 :body "Error 404: Page not found"})))

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
  @(promise))

(comment
  (go)
  (stop)
  (restart) ;; rcf
  ;;
  )
