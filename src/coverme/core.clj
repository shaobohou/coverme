(ns coverme.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as string]
            [clojure.walk :as walk])
  (:use [clojure.pprint])
  (:gen-class))

(defn sanitise-title
  [title]
  (-> (first (string/split title #"[-(\[]"))
      (string/trim)
      (string/replace #"&" "")))

(defn str-to-web
  [s]
  (string/join "%20" (string/split s #" ")))

(defn trim-tracks
  [tracks]
  ;; (println)
  ;; (pprint (take 3 tracks))
  (let [trim-fn (fn [track] (let [artists (first (:artists track))]
                              (merge (select-keys track [:name :popularity])
                                     {:artists (:name artists)
                                      :artists-href (:href artists)
                                      :href (:href track)})))]
    (map trim-fn tracks)))

(defn sort-tracks
  [tracks]
  (reverse (sort-by :popularity tracks)))

(defn unique-tracks
  [field tracks]
  ;; (println)
  ;; (pprint (take 3 tracks))
  (let [groups (group-by field tracks)]
    (map (comp first sort-tracks second) groups)))

(defn get-tracks
  [q]
  (Thread/sleep 200)
  (let [query  (str "http://ws.spotify.com/search/1/track.json?q=" q)
        retval (client/get query {:throw-exceptions false})
        tracks (walk/keywordize-keys (second (second (json/parse-string (:body retval)))))]
    (println (:status retval) query)
    (filter #(> (Double/parseDouble (:popularity %)) 0.20) (trim-tracks tracks))))

(defn get-tracks-by-artists
  "unique by track name"
  [artists]
  (->> (str "artist%3a" (str-to-web artists))
      get-tracks
      (unique-tracks :name)
      (filter #(= (:artists %) artists))
      (map #(assoc % :artists artists))
      sort-tracks))

(defn get-tracks-by-title
  "unique by artists name"
  [full-title]
  (let [title (sanitise-title full-title)]
    (->> (str "track%3a" (str-to-web title))
        get-tracks
        (unique-tracks :artists)
        (filter #(= (:name %) title))
        (map #(assoc % :name title))
        sort-tracks)))

(defn get-cover-tracks
  [track n]
  (->> (get track :name)
       get-tracks-by-title
       (filter #(not= (:artists %) (:artists track)))
       (take n)))

(defn generate-playlist-from-artists
  "Given an artist and a song. Find a more popular/different song by the same artist. Find a different artist that covers the song. Repeat the with new artist and song"
  [artists title]
  (let [artists-tracks (take  5 (filter #(not (.startsWith (:name %) title)) (get-tracks-by-artists artists)))
        cover-tracks   (take 25 (sort-tracks (apply concat (map #(get-cover-tracks % 5) artists-tracks))))
        rand-song      (first (shuffle cover-tracks))]
    (println (count artists-tracks) (count cover-tracks))
    (when rand-song
      (println (:name rand-song) " BY " artists " WAS ALSO COVERED BY " (:artists rand-song))
      (pprint rand-song)
      (println))
    (if rand-song
      (lazy-seq (cons rand-song (generate-playlist-from-artists (:artists rand-song) (sanitise-title (:name rand-song)))))
      '())))

(defn -main
  "I don't do a whole lot."
  [& [artists title ntracks]]
  (let [tracks-by-artists (get-tracks-by-artists "Nina Simone")
        tracks-by-name    (get-tracks-by-title   "I Put A Spell On You")]
    (println)
    (pprint (take 5 tracks-by-artists))
    (println)
    (pprint (take 5 tracks-by-name)))

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
