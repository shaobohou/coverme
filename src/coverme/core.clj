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
  (let [trim-fn (fn [track]
                  (let [artist (-> track (get "artists") first (get "name"))]
                    (merge (select-keys track ["name" "popularity"])
                           {"artist" artist})))]    
    (map trim-fn tracks)))

(defn get-tracks
  [q]
  (let [query (str "http://ws.spotify.com/search/1/track.json?q=" q)]
    (trim-tracks (second (second (json/parse-string (:body (client/get query))))))))

(defn get-tracks-by-artist
  "unique by track name"
  [artist]
  (get-tracks (str "artist%3a" (str-to-web artist))))

(defn get-tracks-by-title
  "unique by artist name"
  [title]
  (get-tracks (str (str-to-web title))))

(defn -main
  "I don't do a whole lot."
  [& x]
  (let [tracks (get-tracks-by-artist "Nina Simone")
        tracks (get-tracks-by-title  "I put a a spell on you")]
    (pprint (take 3 tracks)))
  (pprint (str-to-web "Nina Simone"))
  )
