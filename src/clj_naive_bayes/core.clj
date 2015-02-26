(ns clj-naive-bayes.core
  (:require [clojure.string :refer [split]]))


(defn read-mails
  "Read all files in given folder"
  [dir]
  (let [directory (clojure.java.io/file dir )]
    (for [file (rest (file-seq directory))]
      (slurp file))))


(defn word-probs
  "Compute word frequencies in document list restricted to dictionary entries"
  [doc-list]
  (->> doc-list
       (map #(into #{} (split % #" ")))
       (apply concat)
       frequencies
       (map (fn [[k v]] [k (/ v (count doc-list))]))
       (into {})))


(defn train
  "Train the classifier"
  [spam-dir non-spam-dir]
  (println "Training ...")
  (let [raw-spam (read-mails spam-dir)
        raw-ham (read-mails non-spam-dir)
        dictionary (->> (concat raw-spam raw-ham)
                        (map #(split % #" "))
                        (apply concat)
                        frequencies
                        (remove (fn [[k v]] (< (count k) 2)))
                        (sort-by second >)
                        (take 2500)
                        keys)
        model {:spam-vs-ham (/ (count raw-spam) (count raw-ham))
             :spam-probs (select-keys (word-probs raw-spam) dictionary)
             :ham-probs (select-keys (word-probs raw-ham) dictionary)
             :dictionary dictionary}]
    (println "done")
    model))


(defn classify
  "Classify given document with trained model"
  [doc model]
  (let [doc-words (->> (split doc #" ")
                       (into #{}))]
    (>
     (apply
      +
      (Math/log (:spam-vs-ham model))
      (map
       (fn [w]
         (let [spam-prob (or (get-in model [:spam-probs w]) 0)
               ham-prob (or (get-in model [:ham-probs w]) 0)]
           (if (zero? ham-prob)
             0
             (if (zero? spam-prob)
               0
               (Math/log
                (/ spam-prob
                   ham-prob))))))
       doc-words))
     0)))


(defn test-mails
  "Classify mails in given directory"
  [dir model]
  (println (str "Testing " dir " ..."))
  (let [results (->> (read-mails dir)
                     (map #(classify % model))
                     frequencies)]
    (println (str "Classified as spam: " (or (get results true) 0)))
    (println (str "Classified as nonspam: " (or (get results false) 0)))
    model))


(defn -main [& args]
  (->> (train "data/spam-train" "data/nonspam-train")
       (test-mails "data/spam-test")
       (test-mails "data/nonspam-test"))
  (System/exit 0))


(comment

  (time (->> (train "data/spam-train" "data/nonspam-train")
             (test-mails "data/spam-test")
             (test-mails "data/nonspam-test")
             :done))

  )
