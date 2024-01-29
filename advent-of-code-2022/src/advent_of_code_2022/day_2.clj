(ns advent-of-code-2022.day-2
  (:gen-class)
  (:import (java.io Reader)))

; 1 for Rock A X, 2 for Paper B Y, and 3 for Scissors C Z
; 0 if you lost, 3 if the round was a draw, and 6 if you won
;  X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win



(defn char-seq
  [^Reader rdr]
  (let [chr (.read rdr)]
    (if (>= chr 0)
      (cons chr (lazy-seq (char-seq rdr))))))

(defn read-file-char []
  (with-open [rdr (clojure.java.io/reader "resources/day_2_test_small.txt")]
    (reduce conj [] (char-seq rdr))))

(defn read-file-line []
  (with-open [rdr (clojure.java.io/reader "resources/day_2_test_big.txt")]
    (reduce conj [] (line-seq rdr))))

(defn remove-space-between-letters [vector-of-strings]
  (mapv #(clojure.string/replace % #"\s" "") vector-of-strings))

(defn calculate-points [str-lst]
  (map (fn [pair] (cond
                    (and (= \B (get pair 0)) (= \X (get pair 1))) (+ 1 0)
                    (and (= \C (get pair 0)) (= \Y (get pair 1))) (+ 2 0)
                    (and (= \A (get pair 0)) (= \Z (get pair 1))) (+ 3 0)

                    (and (= \A (get pair 0)) (= \X (get pair 1))) (+ 1 3)
                    (and (= \B (get pair 0)) (= \Y (get pair 1))) (+ 2 3)
                    (and (= \C (get pair 0)) (= \Z (get pair 1))) (+ 3 3)

                    (and (= \C (get pair 0)) (= \X (get pair 1))) (+ 1 6)
                    (and (= \A (get pair 0)) (= \Y (get pair 1))) (+ 2 6)
                    (and (= \B (get pair 0)) (= \Z (get pair 1))) (+ 3 6)

                    :else 0))
       str-lst))

(defn win [char*]
  (cond
    (= \C char*) (+ 1 6)
    (= \A char*) (+ 2 6)
    (= \B char*) (+ 3 6)))

(defn draw [char*]
  (cond
    (= \A char*)  (+ 1 3)
    (= \B char*)  (+ 2 3)
    (= \C char*)  (+ 3 3)))

(defn loose [char*]
  (cond
    (= \B char*) (+ 1 0)
    (= \C char*) (+ 2 0)
    (= \A char*) (+ 3 0)))


(defn decode-action [str-lst]
  (map (fn [pair]
         (cond
           (= \Z (get pair 1)) (win (get pair 0))
           (= \Y (get pair 1)) (draw (get pair 0))
           (= \X (get pair 1)) (loose (get pair 0))))
       str-lst))

(defn run-day-2 []
  (->> (read-file-line)
       (remove-space-between-letters)
       (decode-action)
       (apply +)))

