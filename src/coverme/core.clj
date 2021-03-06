(ns coverme.core
  (:require [clj-http.client :as client]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [hiccup.form :as form]
            [cheshire.core :as json]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clj-time.core :as time])
  (:use [compojure.core]
        [hiccup.core]
        [hiccup.form :only (form-to label text-area text-field submit-button)]
        [clojure.pprint])
  (:gen-class))

(defn sanitise-title
  [title]
  (-> (first (string/split title #"[-(\[]"))
      (string/trim)
      (string/replace #"&" "")))

(defn trim-tracks
  [tracks]
  ;; (println)
  ;; (pprint (take 3 tracks))
  (let [trim-fn (fn [track] (let [artists (first (:artists track))]
                              {:artists (:name artists)
                               :artists-href (:href artists)
                               :name (sanitise-title (:name track))
                               :name-raw (:name track)
                               :href (:href track)
                               :popularity (:popularity track)}))]
    (map trim-fn tracks)))

(defn sort-tracks
  [tracks]
  (reverse (sort-by :popularity tracks)))

(defn unique-tracks
  [tracks]
  ;; (println)
  ;; (pprint (take 3 tracks))
  (let [groups (group-by #(map string/lower-case (map % '(:artists :name))) tracks)]
    (map (comp first sort-tracks second) groups)))

(defn get-with-retry
  [query nretry]
  (let [     tries (repeatedly nretry #(client/get query {:throw-exceptions false}))
         bad-tries (take-while #(not= 200 (:status %)) tries)
        good-tries (drop-while #(not= 200 (:status %)) tries)]
    (when (pos? (count bad-tries)) (println "TRIED" (inc (count bad-tries)) "TIMES FOR " query))
    (if (empty? good-tries)
      (last   bad-tries)
      (first good-tries))))

(defn get-tracks
  [q]
  (Thread/sleep 200)
  (let [query  (str "http://ws.spotify.com/search/1/track.json?" q)
        retval (get-with-retry query 5)
        {:keys [info tracks]} (walk/keywordize-keys (json/parse-string (:body retval)))]
    (println (:status retval) query)
    (filter #(> (Double/parseDouble (:popularity %)) 0.2) (trim-tracks tracks))))

(defn get-tracks-by-artists
  "unique by track name"
  [artists]
  (->> (client/generate-query-string {"q" (str "artist:" artists)})
      get-tracks
      unique-tracks
      (filter #(= (:artists %) artists))
      (map #(assoc % :artists artists))
      sort-tracks))

(defn get-tracks-by-title
  "unique by artists name"
  [title]
  (->> (client/generate-query-string {"q" (str "track:" title)})
       get-tracks
       unique-tracks
       (filter #(= (:name %) title))
       (map #(assoc % :name title))
       sort-tracks))

(defn get-cover-tracks
  [track n]
  (->> (get track :name)
       get-tracks-by-title
       (filter #(not= (:artists %) (:artists track)))
       (take n)))

(defn generate-playlist-from-artists
  "Given an artist and a song. Find a more popular/different song by the same artist. Find a different artist that covers the song. Repeat the with new artist and song"
  [artists title]
  (let [artists-tracks (take  5 (filter #(not (.startsWith (string/lower-case (:name %)) (string/lower-case title)))
                                        (get-tracks-by-artists artists)))
        cover-tracks   (take  5 (sort-tracks (mapcat #(get-cover-tracks % 5) artists-tracks)))
        rand-song      (first (shuffle cover-tracks))]
    (println (count artists-tracks) (count cover-tracks))
    (when rand-song
      (println (:name rand-song) " BY " artists " WAS ALSO COVERED BY " (:artists rand-song))
      (pprint rand-song)
      (println))
    (if rand-song
      (lazy-seq (cons rand-song (generate-playlist-from-artists (:artists rand-song) (:name rand-song))))
      '())))

(defn format-playlist
  [tracks]
  (->> tracks
       (map #(html [:a {:href (:href %)} (:name-raw %)] [:br]
                   "by " [:a {:href (:artists-href %)} (:artists %)] [:br]
                   [:a {:href (:href %)} (:href %)]))
       (interpose "<p>")))

(defn generate-playlist
  [artist track maxlen]
  (spit "coverme.log" (string/join " " [(time/now) artist track maxlen "\n"]) :append true)
  (take (Integer/parseInt (if (empty? maxlen) "10" maxlen))
        (generate-playlist-from-artists (if (empty? artist) "Nina Simone" artist)
                                        (if (empty? track) "???" track))))

(defroutes app-routes
  (GET "/" [] (html [:div {:id "coverme-form" :class "coverme"}
                     (form-to [:get "/cover"]
                              (text-field "artist" "Nina Simone")
                              (label "artist" "Artist")
                              [:br]
                              (text-field "track" "???")
                              (label "track" "Track")
                              [:br]
                              (text-field "maxlen" "10")
                              (label "maxlen" "Num. Songs")
                              [:br]
                              (submit-button "Cover Me!"))]))
  (GET "/cover" {{artist :artist track :track maxlen :maxlen} :params}
       (format-playlist (generate-playlist artist track maxlen)))
  (route/resources "/")
  (route/not-found "<h1>Page not found</h1>"))

(defn wrap-exceptions [app]
  (fn [req] (try (app req)
                 (catch IllegalArgumentException err
                   {:status 400 :body (.getMessage err)})
                 (catch Exception err
                   {:status 500 :body (.getMessage err)}))))

(def app
  (-> app-routes
      handler/site
      wrap-exceptions))

(defn -main
  "I don't do a whole lot."
  [& [artists title ntracks]]
  ;; (let [tracks-by-artists (get-tracks-by-artists "Nina Simone")
  ;;       tracks-by-name    (get-tracks-by-title   "I Put A Spell On You")]
  ;;   (println)
  ;;   (pprint (take 5 tracks-by-artists))
  ;;   (println)
  ;;   (pprint (take 5 tracks-by-name)))

  (println (client/generate-query-string {"q" ":Me & You"}))

  (println "\n\n\n")
  (pprint (str artists))
  (pprint (str title))
  (pprint (Integer/parseInt ntracks))
  (println "\n\n\n")
  (println (sanitise-title "Secret Crush (feat. Jeff Lorber)"))
  (println (sanitise-title "Secret Crush - Blah"))
  (println (sanitise-title "Secret Crush / Blah"))
  ;; (generate-playlist-from-artists "Nina Simone" "_")
  (println "\n\n\n")
  (let [retval (take (Integer/parseInt ntracks) (generate-playlist-from-artists artists title))]
    (pprint (count retval)))
  (println "\n\n\nFinished!!!!")
  )
