(ns coverme.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as string])
  (:use [clojure.pprint])
  (:gen-class))

(defn trim-title
  [title]
  (string/trim (first (string/split title #"[-(/]"))))

(defn str-to-web
  [s]
  (string/join "%20" (string/split s #" ")))

(defn trim-tracks
  [tracks]
  ;; (println)
  ;; (pprint (take 3 tracks))
  (let [trim-fn (fn [track]
                  (let [artists (-> track (get "artists") first)
                        href (get track "href")]
                    (merge (select-keys track ["name" "popularity"])
                           {"artists" (get artists "name")
                            ;; "artists-href" (get artists "href")
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
  (Thread/sleep 1000)
  (let [query (str "http://ws.spotify.com/search/1/track.json?q=" q)]
    (trim-tracks (second (second (json/parse-string (:body (client/get query))))))))

(defn get-tracks-by-artists
  "unique by track name"
  [artists]
  (-> (str "artist%3a" (str-to-web artists))
      get-tracks
      (unique-tracks "name")
      ((fn [tracks] (map #(assoc % "artists" artists) tracks)))
      sort-tracks))

(defn get-tracks-by-title
  "unique by artists name"
  [full-title]
  (let [title (trim-title full-title)]
    (-> (str "track%3a" (str-to-web title))
        get-tracks
        (unique-tracks "artists")
        ((fn [tracks] (map #(assoc % "name" title) tracks)))
        sort-tracks)))

(defn generate-playlist-from-artists
  "Given an artist and a song. Find a more popular/different song by the same artist. Find a different artist that covers the song. Repeat the with new artist and song"
  [artists title]
  (let [artists-tracks (take 5 (filter #(not= (get % "name") title) (get-tracks-by-artists artists)))
        rand-song      (first (shuffle artists-tracks))
        new-title      (trim-title (get rand-song "name"))
        title-tracks   (take 5 (filter #(not= (get % "artists") artists) (get-tracks-by-title new-title)))
        rand-song2     (first (shuffle title-tracks))
        new-artists    (get rand-song2 "artists")
        ]
    (println)
    (println new-title " BY " artists " WAS ALSO COVERED BY " new-artists)
    (pprint rand-song2)
    (println)
    (cons rand-song
          (lazy-seq (generate-playlist-from-artists new-artists new-title)))))

(defn -main
  "I don't do a whole lot."
  [& [artists title ntracks]]
  ;; (let [tracks-by-artists (get-tracks-by-artists "Nina Simone")
  ;;       tracks-by-name    (get-tracks-by-title   "I Put A Spell On You")]
  ;;   (println)
  ;;   (pprint (take 5 tracks-by-artists))
  ;;   (println)
  ;;   (pprint (take 5 tracks-by-name)))

  (println "\n\n\n")
  (pprint (str artists))
  (pprint (str title))
  (pprint (Integer/parseInt ntracks))
  (println "\n\n\n")
  (println (trim-title "Secret Crush (feat. Jeff Lorber)"))
  (println (trim-title "Secret Crush - Blah"))
  (println (trim-title "Secret Crush / Blah"))
  (println "\n\n\n")
  (print (count (take (Integer/parseInt ntracks) (generate-playlist-from-artists artists title))))
  (println "\n\n\nFinished!!!!")
  )
