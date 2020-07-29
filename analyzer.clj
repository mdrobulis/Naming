#!/usr/bin/env bb
(defn non-ascii [word]

  )

(defn single-word [word]
(if (re-find #"^[^\s]+$" word) true false)
)

(defn special-chars [word]
  (if (re-find #"[^\w]" word) true false)
  )

(defn sorted? [words]
  (= words (sort words)))

(defn stats [words]
  {:count (count words)
   :max-length (apply max (map count words))
   :min-length (apply min (map count words))
   :distinct (apply distinct? words)
   :single-words (every? single-word words)
   :special-chars (some special-chars words)
   :empty-lines (some empty? words)
   :sorted (sorted? words)
   }
  )

(defn quality [stats]
  (let [fns [#(if (:sorted %) 1 0) 
             #(if (:empty-lines %)  -1 0)
             #(if (:single-words %) 1 0)
             #(if (:special-chars %) -1 0)
             #(if (:distinct %) 1 0)
             #(if (> 100 (:count %)) 1 0)
             #(if (> 12 (:max-length %)) 1 -1)
             #(if (< 2 (:min-length %)) 1 -1)]
        vals (map #(or (% stats) 0) fns)        
        ]
    (reduce + vals)
    ))




(defn analyze-file [file]
  (let [text (slurp file)
        words (into [] (.split  text "\n"))
        stats (stats words)
        ext (assoc stats :file file
                   :quality (quality stats))        
        ]
    ext)
  )

(defn md-link [stat]
  (str "[" (:file stat)"]("(:file stat) ")"))


(defn file-stats-text [stats]
  (str
   "## " (md-link stats) " \n"
   "- quality : " (:quality stats) "\n"
   "- count : " (:count stats) "\n"
   "- sorted : " (if (:sorted stats) "yes" "no" ) "\n"
   "\n"
   )
  )

(defn markdown-stats-text [stats]
  (let [largest   (last (sort-by :count stats))
        best  (last (sort-by :quality stats))
        ]
    (str
     "# Names index \n"
     " - Total lists : " (count stats) "\n"
     " - Total names : " (reduce + (map :count stats)) "\n"
     " - Largest list : " (md-link largest) " - " (:count largest) " total \n"
     " - Best quality : " (md-link best) "- " (:quality best) " score \n" 
     (apply str (map file-stats-text stats)))))



(defn dir-files [dir]
  (->> (clojure.java.io/file dir)
       file-seq
       (filter #(.isFile %))
       (filter #(.endsWith (str % ) ".csv"))
       (mapv #(.getPath %))
       )
  )

(defn get-file-stats [files]
   (map analyze-file files))

(defn print-markdown-index [stats]
  (println "Printing index.md...")
  (spit "index.md" (markdown-stats-text stats))) 

(defn print-json-index [stats]
(println "Printing index.json ...")
(spit "index.json" (json/generate-string stats)))

(defn print-edn-index [stats]
  (println "Printing index.edn ...")
  (spit "index.edn" (str stats))
  )



                   (defn index! []
                     (-> "." dir-files get-file-stats print-markdown-index))






                   (index!)

