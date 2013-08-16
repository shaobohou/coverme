(ns coverme.core
  (:require [clj-http.client :as client]
            [cheshire.core :as json]
            [clojure.string :as string])
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
  (let [trim-fn (fn [track]
                  (let [artists (-> track (get "artists") first)
                        href (get track "href")]
                    (merge (select-keys track ["name" "popularity"])
                           {"artists" (get artists "name")
                            ;; "artists-href" (get artists "href")
                            "href" (get track "href")
                            })))]
    (map trim-fn tracks)))

(defn sort-tracks
  [tracks]
  (reverse (sort-by #(get % "popularity") tracks)))

(defn unique-tracks
  [field tracks]
  ;; (println)
  ;; (pprint (take 3 tracks))
  (let [groups (group-by #(get % field) tracks)]
    (map (comp first sort-tracks second) groups)))

(defn get-tracks
  [q]
  (Thread/sleep 200)
  (let [query  (str "http://ws.spotify.com/search/1/track.json?q=" q)
        tracks (second (second (json/parse-string (:body (client/get query)))))]
    ;; (println query)
    (filter #(> (Double/parseDouble (get % "popularity")) 0.10) (trim-tracks tracks))))

(defn get-tracks-by-artists
  "unique by track name"
  [artists]
  (->> (str "artist%3a" (str-to-web artists))
      get-tracks
      (unique-tracks "name")
      (filter #(= (get % "artists") artists))
      (map #(assoc % "artists" artists))
      sort-tracks))

(defn get-tracks-by-title
  "unique by artists name"
  [full-title]
  (let [title (sanitise-title full-title)]
    (->> (str "track%3a" (str-to-web title))
        get-tracks
        (unique-tracks "artists")
        (filter #(= (get % "name") title))
        (map #(assoc % "name" title))
        sort-tracks)))

(defn generate-playlist-from-artists
  "Given an artist and a song. Find a more popular/different song by the same artist. Find a different artist that covers the song. Repeat the with new artist and song"
  [artists title]
  (let [artists-tracks (take 5 (filter #(not (.startsWith (get % "name") title)) (get-tracks-by-artists artists)))
        cover-tracks  (sort-tracks (apply concat (map (fn [track] (->> (get track "name")
                                                                        get-tracks-by-title
                                                                        (filter #(not= (get % "artists") artists))
                                                                        (take 5)))
                                                       artists-tracks)))
        rand-song   (first (shuffle (take 5 cover-tracks))) ;; different song by a different artist
        new-artists (get rand-song "artists")
        new-title   (get rand-song "name")]
    (println new-title " BY " artists " WAS ALSO COVERED BY " new-artists)
    (pprint rand-song)
    (println)
    (lazy-seq (cons rand-song
                    (generate-playlist-from-artists new-artists (sanitise-title new-title))))))

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
  (generate-playlist-from-artists "Nina Simone" "_")
  (println "\n\n\n")
  (let [retval (take (Integer/parseInt ntracks) (generate-playlist-from-artists artists title))]
    (pprint (count retval)))
  (println "\n\n\nFinished!!!!")
  )
