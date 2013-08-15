(ns coverme.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as string])
  (:use [clojure.pprint])
  (:gen-class))

(defn str-to-web
  [s]
  (string/join "%20" (string/split s #" ")))

(defn trim-tracks
  [tracks]
  ;; (println)
  ;; (pprint (take 3 tracks))
  (let [trim-fn (fn [track]
                  (let [artist (-> track (get "artists") first)
                        href (get track "href")]
                    (merge (select-keys track ["name" "popularity"])
                           {"artists" (get artist "name")
                            ;; "artists-href" (get artist "href")
                            ;; "name-href" (get track "href")
                            })))]
    (map trim-fn tracks)))

(defn sort-tracks
  [tracks]
  (reverse (sort-by #(get % "popularity") tracks)))

(defn unique-tracks
  [tracks field]
  ;; (println)
  ;; (pprint (take 3 tracks))
  (let [groups (group-by #(get % field) tracks)]
    (map (comp first sort-tracks second) groups)))

(defn get-tracks
  [q]
  (let [query (str "http://ws.spotify.com/search/1/track.json?q=" q)]
    (trim-tracks (second (second (json/parse-string (:body (client/get query))))))))

(defn get-tracks-by-artist
  "unique by track name"
  [artist]
  (-> (str "artist%3a" (str-to-web artist))
      get-tracks
      (unique-tracks "name")
      ((fn [tracks] (map #(assoc % "artists" artist) tracks)))
      sort-tracks))

(defn get-tracks-by-title
  "unique by artist name"
  [title]
  (-> (str "track%3a" (str-to-web title))
      get-tracks
      (unique-tracks "artists")
      ((fn [tracks] (map #(assoc % "name" title) tracks)))
      sort-tracks))

(defn -main
  "I don't do a whole lot."
  [& x]
  (let [tracks-by-artist (get-tracks-by-artist "Nina Simone")
        tracks-by-name   (get-tracks-by-title  "I Put A Spell On You")]
    (println)
    (pprint (take 5 tracks-by-artist))
    (println)
    (pprint (take 5 tracks-by-name)))
  )
