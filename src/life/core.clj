(ns life.core
  (:gen-class))

(def WORLD
  ; Blinker
  ;[[0 0 0 0 0]
  ; [0 0 1 0 0]
  ; [0 0 1 0 0]
  ; [0 0 1 0 0]
  ; [0 0 0 0 0]]

  ; Toad
  ;[[0 0 0 0 0 0]
  ; [0 0 0 1 0 0]
  ; [0 1 0 0 1 0]
  ; [0 1 0 0 1 0]
  ; [0 0 1 0 0 0]
  ; [0 0 0 0 0 0]]

  ; Glider
  [[0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 1 0 0 0 0 0 0 0 0 0]
   [0 0 0 1 0 0 0 0 0 0 0 0]
   [0 1 1 1 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0 0 0 0 0]])

(defn get-neighbors [w x y]
  (filter (comp not nil?)
    [(get-in w [(- y 1) (- x 1)])
     (get-in w [(- y 1) x])
     (get-in w [(- y 1) (+ x 1)])
     (get-in w [y       (- x 1)])
     (get-in w [y       (+ x 1)])
     (get-in w [(+ y 1) (- x 1)])
     (get-in w [(+ y 1) x])
     (get-in w [(+ y 1) (+ x 1)])]))

(defn cell-fate [cell live-neighbors]
  (if (= cell 1)
    (cond
      ;; Rule 1
      (< live-neighbors 2)      0
      ;; Rule 2
      (or (= live-neighbors 2)
          (= live-neighbors 3)) 1
      ;; Rule 3
      (> live-neighbors 3)      0
    ;; Rule 4
    (if (= live-neighbors 3)
      1
      0)))

(defn next-cell [world x y]
  {:pre [(= (count world) (count WORLD))
         (= (count (filter (comp not nil?) (get world 0))) (count (get WORLD 0)))
         (and (>= x 0) (< x (count (get WORLD 0))))
         (and (>= y 0) (< y (count WORLD)))]}
  (let [cell (get-in world [y x])
        live-neighbors (filter (comp not zero?) (get-neighbors world x y))]
    (cell-fate cell (count live-neighbors))))

(defn with-coordinates [world]
  (for [y (range (count world))]
    (for [x (range (count (get world y)))]
      [(get-in world [y x]) [x y]])))

(defn print-world [world]
  (doseq [y (range (count world))]
    (let [ch (mapv #(if (zero? (get-in world [y %]))
                      \space
                      \X)
                   (range (count (get world y))))
          out (apply str (interpose \space (vec ch)))]
      (println out))))

(defn tick [world]
  {:pre  [(= (count world) (count WORLD))
          (= (count (get world 0)) (count (get WORLD 0)))]
   :post [(= (count %) (count world))
          (= (count (get % 0)) (count (get world 0)))]}
  (mapv (fn [row]
          (mapv (fn [[cell [x y]]]
                  (next-cell world x y))
                row))
        (with-coordinates world)))

(defn -main
  [& args]
  (let [t 500]
    (loop [w WORLD]
      (print-world w)
      (println)
      (. Thread (sleep t))
      (recur (tick w)))))
