(ns advent-of-code.day_1)

(def test-data [1000
                2000
                3000

                4000

                5000
                6000

                7000
                8000
                9000

                10000])

(comment

  (defn read-input []
      (let [input (read-line)]
           (try
             (if (= input "stop")
               "stop"
               (Integer/parseInt input))
             (catch NumberFormatException e
               (println "Invalid move! Input most be an number. Try again.")))))

  (defn move
        [board player-x?]
        (loop [board board
               player-x? player-x?]
              (let [input (read-input)]
                   (cond
                     (= input "stop") (str "Game has stopped")
                     (and (valid-number? input) (valid-field? input board)) (update-board input board player-x?)
                     :else
                     (recur board player-x?))))))
