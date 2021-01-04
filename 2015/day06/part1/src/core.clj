(ns core
  (:require [clojure.string :as str])
  (:require [clojure.core.matrix :as matrix]))

(defn read-file [p]
  (str/split (slurp p) #"\n"))

(defn parse-instr [instr]
  (def instr-seq (str/split instr #" "))
  (if (= (first instr-seq) "turn")
    [
      (str (nth instr-seq 0) (nth instr-seq 1))
      (map read-string (str/split (nth instr-seq 2) #","))
      (map read-string (str/split (nth instr-seq 4) #","))
    ]
    [
      (nth instr-seq 0)
      (map read-string (str/split (nth instr-seq 1) #","))
      (map read-string (str/split (nth instr-seq 3) #","))
    ]))

(defn turn [matrix coords p]
  (reduce (fn [m xy] (matrix/mset m (first xy) (second xy) (p m xy))) matrix coords))

(defn toggle [matrix coords]
  (turn matrix coords (fn [m xy] (not (matrix/mget m (first xy) (second xy))))))

(defn get-coords [from to]
  (def r (map
    (fn [x] (range (first x) (inc (second x))))
    (map vector from to)))
      (for [x (first r) y (second r)] [x, y]))

(defn calculate [m]
 (matrix/ereduce + (matrix/emap {false 0 true 1} m)))

(defn run [matrix instr]
  (def from (nth instr 1))
  (def to (nth instr 2))
  (case (first instr)
    "turnon" (turn matrix (get-coords from to) (constantly true))
    "turnoff" (turn matrix (get-coords from to) (constantly false))
    "toggle" (toggle matrix (get-coords from to))
  ))

(defn -main [& args]
  (def matrix (matrix/matrix (make-array Boolean/TYPE 1000 1000)))
  (if (seq args)
    (println (calculate (reduce (fn [m instr] (run m (parse-instr instr))) matrix (read-file (first args)))))
    (throw (Exception. "Usage: lein run <input>"))))