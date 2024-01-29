(ns advent-of-code-2022.day-1
  (:gen-class))

;

(defn read-file []
  (with-open [rdr (clojure.java.io/reader "resources/day_1_test_big.txt")]
    (reduce conj [] (line-seq rdr))))

(defn day-1 []
  (->> (read-file)
       (partition-by #(= "" %))                             ;Split by "". Eks (("1" "2") ("") ("5"))
       (filter #(not= "" (first %)))                        ;Remove list with empty string ("")
       (map (fn [lst]
              (->> lst
                   (map #(Integer/parseInt %))
                   (apply + ))))                            ;Convert to int and sum each list. Eks (3 5)
       (sort >)
       (take 3)
       (apply +)))

