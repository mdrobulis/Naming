
(defn dir-files [dir]
  (->> (clojure.java.io/file dir)
       file-seq
       (filter #(.isFile %))
       (filter #(.endsWith (str % ) ".csv"))
       (mapv #(.getPath %))
       )
  )

(defn get-words[file]
  (let [text (slurp file)
        words (into [] (.split  text "\n"))
        ]
    words)
  )

(def p (mapcat get-words))

(def w (into [] p (dir-files ".")))

(doseq [ w (take 20 (shuffle w))]
  (println w)
  (println)
  )



