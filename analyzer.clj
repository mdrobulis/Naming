#!/usr/bin/env bb
(defn non-ascii [word]

  )

(defn single-word [word]
(if (re-find #"^[^\s]+$" word) true false)
)

(defn special-chars [word]
  (if (re-find #"[^\w]" word) true false)
  )

(defn duplicates [words]
  (->> words
       frequencies
       (filter #(< 1 (last %)))
       keys
       ))


(defn sorted? [words]
  (= words (sort words)))

(defn- var-from-sums
  [ss s cnt]
  (when (<= cnt 0)
    (throw (ex-info "Most have one or more samples to compute variance." {:causes #{:div-by-zero :bad-args}})))
  (double (/ (- ss (/ (* s s) cnt)) cnt)))

(defn calculate-variance3
  [sample]
  (let [tallies (reduce 
                 (fn [tallies n] 
                   {:sumsq (+ (:sumsq tallies) (* n n)) 
                    :sum   (+ (:sum   tallies) n)})
                 {:sumsq 0 :sum 0} 
                 sample)]
    (var-from-sums (:sumsq tallies) (:sum tallies) (count sample))))


(defn stats [words]
  (let [init {:count (count words)
              :max-length (apply max (map count words))
              :min-length (apply min (map count words))
              :avg-length (/ (apply + (map count words))  (count words) 1.0 )
              :distinct (apply distinct? words)
              :single-words (count (filter single-word words))              
              :special-chars (count (filter special-chars words))
              :empty-lines (count (filter empty? words))
              :sorted (sorted? words)
              :dups (duplicates words)
              }
        ]
    (-> init
        (assoc :mul-words (- (:count init) (:single-words init)))
        (assoc :dup-count (count (:dups init)))
        (assoc :variance  (calculate-variance3 (map count words)))
        #_(assoc :shit-ratio (/ (* 10000 (+ ((juxt :mul-words
                                                   :empty-lines
                                                   :special-chars                                                                                 :dup-count
                                                   ) init))                                               
                                   )
                                (:count init)
                                ))
        )
    ))

(defn quality [stats]
  (let [fns [#(if (:sorted %) 5 0) 
             #(* -10 (:empty-lines %) )
                                        ;             #(:single-words %) 
             #(* -1 (:special-chars %))
             #(if (:distinct %) (:sigle-words %)  0)
                                        ;             #(:count %)
             #(if (> 12 (:max-length %)) 5 0)
             #(if (< 2 (:min-length %)) 5 0)]
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


  (defn yn [b] (if b "yes" "no"))


(defn file-stats-text [stats]
  (str
   "## " (md-link stats) " \n"
   "- quality     : " (:quality stats) "\n"
   "- count       : " (:count stats) "\n"
   "- sorted      : " (yn (:sorted stats)) "\n"
   "- distinct    : " (yn (:distinct stats)) "\n"
   "- one word    : " (:single-words stats) "\n"
   "- special char: " (:special-chars stats) "\n"
   "- empty-lines : " (:empty-lines stats) "\n"
   "- max-length  : " (:max-length stats) "\n"
   "- avg-length  : " (:avg-length stats) "\n"
   "- variance    : " (:variance stats) "\n"
   "- dups        : " (count (:dups stats)) " " (:dups stats) "\n" 
   "\n"
   )
  )

(defn markdown-stats-text [stats]
  (let [largest   (last (sort-by :count stats))
        best  (last (sort-by :quality stats))
        ]
    (str
     "# General stats \n\n"
     " - Total lists : " (count stats) "\n"
     " - Total names : " (reduce + (map :count stats)) "\n"
     " - Largest list ("(:count largest)") : " (md-link largest) "\n"
     " - Best quality ("(:quality best)") : " (md-link best) "\n\n"
     "## [mdrobulis/Naming](https://github.com/mdrobulis/Naming) index \n\n"
     (apply str (map file-stats-text stats))
     "\n\n##  "
     )))



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
  (spit "index.edn" (with-out-str  (clojure.pprint/pprint stats *out*  )))
  )


(defn index! []
  (->> "." dir-files get-file-stats
       (sort-by :quality )
       reverse
       ((juxt print-markdown-index print-json-index print-edn-index ) )))

(index!)

