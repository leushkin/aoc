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

(defn turn [matrix coords f]
  (reduce (fn [m xy] (matrix/mset m (first xy) (second xy) (f m xy))) matrix coords))

(defn m-incr [matrix coords f]
  (turn matrix coords (fn [m xy] (f (matrix/mget m (first xy) (second xy))))))

(defn get-coords [from to]
  (def r (map
    (fn [x] (range (first x) (inc (second x))))
    (map vector from to)))
      (for [x (first r) y (second r)] [x, y]))

(defn calculate [m]
 (matrix/ereduce + (matrix/emap + m)))

(defn run [matrix instr]
  (def from (nth instr 1))
  (def to (nth instr 2))
  (case (first instr)
    "turnon" (m-incr matrix (get-coords from to) inc)
    "turnoff" (m-incr matrix (get-coords from to) (fn [x] (if (= x 0) 0 (- x 1))))
    "toggle" (m-incr matrix (get-coords from to) (fn [x] (+ x 2)))))

(defn -main [& args]
  (def matrix (matrix/matrix (make-array Integer/TYPE 1000 1000)))
  (if (seq args)
    (println (calculate (reduce (fn [m instr] (run m (parse-instr instr))) matrix (read-file (first args)))))
   (throw (Exception. "Usage: lein run <input>"))))
