(ns advent-of-code-2022.day-3
  (:gen-class)
  (:import (java.io Reader)))


;; Priorities for these items must still be found to organize the sticker attachment efforts: here, they are 18 (r) for the first group and 52 (Z) for the second group. The sum of these is 70.
;todo 1. read 3 lines at a time

;todo 2. prioritize each letter of 3 words

;todo 4. find item that appears in all 3 compartments

;todo 5. sum the items

;Book, Chapter, Paragraph, Sentence, Word, Letter

(defn read-line-from-file []
  (with-open [rdr (clojure.java.io/reader "resources/day_3_test_small.txt")]
    (reduce conj [] (line-seq rdr))))

(defn split-sentence-in-two [str-lst]
  (map #(split-at (/ (count %) 2) %) str-lst))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn char->keyword [char-lst]
  (map #(keyword (str %)) char-lst))

(defn priority-schema []
  (let [a-z (char->keyword (char-range \a \z))
        A-Z (char->keyword (char-range \A \Z))
        a-Z (concat a-z A-Z)]
    (zipmap a-Z (range 1 53))))

(defn give-letters-priority [sentence-block]
  (let [letter->priority-int (fn [letter] ((keyword (str letter)) (priority-schema)))
        sentence (fn [sentence] (map letter->priority-int sentence))
        sentence-list (fn [sentence-lst] (map sentence sentence-lst))]
    (map sentence-list sentence-block)))

(defn find-duplicate [sentence-block]
  (map (fn [word] (some (set (first word)) (second word))) sentence-block))

(defn str->char [str]
  (seq (chars (char-array str))))


(defn str-block->char-block [str-block]
  (let [str-lst->char-lst (fn [str-lst] (map str->char str-lst))]
    (map str-lst->char-lst str-block)))

(defn letter->priority [letter]
  ((keyword (str letter)) (priority-schema)))


(defn run-day-3-part-1 []
  (->> (read-line-from-file)
       (split-sentence-in-two)
       (give-letters-priority)
       (find-duplicate)
       (apply +)))

(defn run-day-3-part-2 []
  (->> (read-line-from-file)
       (partition 3)
       (str-block->char-block)
       (mapcat (fn [blok] (->> blok
                            (map (fn [word]
                                   (->> word
                                   (map #(letter->priority %))
                                   set)))
                            (reduce (fn [acc word] (clojure.set/intersection acc word))))))
       (apply +)))