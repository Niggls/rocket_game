(ns rocket-game.test
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [clj-audio.core :refer :all]))

;; function for reset state
(defn reset-state-variable []
 {:rocket {:image (q/load-image "images/rocket.png")
           :x 250
           :y 250
           :dir 0}
  :background (q/load-image "images/stars.jpg")
  :fires []
  :score 0
  :meteors []})

;; setup: here we define our global state variable
(defn setup []
  ;; these two lines, a map (data structure) is added in step 1-6
  {:rocket {:image (q/load-image "images/rocket.png")
            :x 250
            :y 250
            :dir 0}
   :background (q/load-image "images/stars.jpg")
   :fires []
   :score 0
   :meteors []})

;;;; helper methods;;;;;;;;;;;;;;;;;;;;;;;;
(defn inside? [x y]
  (or
   (< x -5)
   (= (+ x 40) 600)
   (= y 0)
   (= (+ y 40) 700)))

(defn fire-inside? [bullet]
 (let [x (:x bullet)
       y (:y bullet)]
  (< y 0)))

(defn meteor-inside? [meteor]
 (let [x (:x meteor)
       y (:y meteor)]
  (> y 700)))

(defn fire-hit-meteor? [state meteor]
 (let [fires (:fires state)
       x (:x meteor)
       y (:y meteor)]
  (contains? fires {:x x :y y})))

(defn remove-fire [fires]
 (remove fire-inside? fires))

(defn meteor-out [state]
  (let [old (count (:meteors state))
        new-meteor (remove meteor-inside? (:meteors state))
        new (count new-meteor)]
   {:rocket (:rocket state)
    :background (q/load-image "images/stars.jpg")
    :fires (:fires state)
    :score (+ (:score state) (- old new))
    :meteors new-meteor}))

(defn meteor-hit [state]
 (let [rocket-x (:x (:rocket state))
       rocket-y (:y (:rocket state))
       meteors (:meteors state)]
   (if (empty? meteors)
       state
       (if (loop [[m1 & rest] meteors]
             (if (or (and
                       (<= (:x m1) rocket-x (+ (:x m1) 45))
                       (<= (:y m1) rocket-y (+ (:y m1) 45)))
                     (and
                       (<= (:x m1) (+ rocket-x 45) (+ (:x m1) 45))
                       (<= (:y m1) (+ rocket-y 45) (+ (:y m1) 45))))
               true
               (if (empty? rest)
                   false
                   (recur rest))))
           (reset-state-variable)
           state))))
(defn fire-hit-meteor? [fire meteors]
  (let [fire-x (:x fire)
        fire-y (:y fire)]
    (loop [[m1 & rest] meteors]
     (if (and
          (<= (:x m1) fire-x (+ (:x m1) 45))
          (<= (:y m1) fire-y (+ (:y m1) 45)))
         m1
         (if (empty? rest)
             0
             (recur rest))))))


(defn fire-hit [state]
 (let [fires (:fires state)
       meteors (:meteors state)
       meteors-to-remove []
       fires-to-remove []]
   (if (or (empty? meteors) (empty? fires))
       state
       (do
         (loop [[f1 & rest] fires]
           (when-not (= (fire-hit-meteor? f1 meteors) 0)
                     (conj fires-to-remove f1)))

         {:rocket (:rocket state)
          :background (q/load-image "images/stars.jpg")
          :fires (remove true fires-to-remove)
          :score (:score state)
          :meteors (:meteors state)}))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; creation methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn create-fire [state]
 (let [x  (:x (:rocket state))
       y  (:y (:rocket state))]
   (update-in state [:fires] conj {:x (+ x 22) :y (- y 20)})))

(defn create-meteor [state]
 (if (= (rand-int 10) 1)
     (if-not (= (:dir (:rocket state)) 0)
             (update-in state [:meteors] conj {:x (rand-int (+ (q/width) -40)) :y -40 :speed (+ (rand-int 10) 5)})
             ;(update-in state [:meteors] conj {:x (rand-int (+ (q/width) -40)) :y -40 :speed 1})
             state)
     state))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; reset methods;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset-game [state]
  (if
    (inside? (:x (:rocket state )) (:y (:rocket state)))
    (reset-state-variable)
    state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

;;;;;;;; move methods;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move [state event]
  (case (:key event)
    (:w :up) (assoc-in state [:rocket :dir] 1)
    (:s :down) (assoc-in state [:rocket :dir] 2)
    (:a :left) (assoc-in state [:rocket :dir] 3)
    (:d :right) (assoc-in state [:rocket :dir] 4)
    ;(:x)     (assoc-in state [:rocket :x] 300)
    ;(:y)     (assoc-in state [:rocket :y] 350)
    (:f) (create-fire state)
    state))

(defn move-fire [fire]
  (update-in fire [:y] #(- % 10)))

(defn move-meteors [meteor]
 (let [speed (:speed meteor)]
  (update-in meteor [:y] #(+ % speed))))


(defn move-rocket [rocket]
 (case (:dir rocket)
  (1) (update-in  rocket [:y] - 10)
  (2) (update-in  rocket [:y] + 10)
  (3) (update-in  rocket [:x] - 10)
  (4) (update-in  rocket [:x] + 10)
  (0) (update-in  rocket [:x] + 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; draw method
(defn draw [state]
  ;; q/background-image and q/image functions are added in step 1-6
  (q/background-image (:background state))
  (q/image (:image (:rocket state))
           (:x (:rocket state))
           (:y (:rocket state)))
  (q/fill 0 0 255)
  (q/stroke 0 0 255)
  (doseq [fire (:fires state)]
   (q/rect (:x fire) (:y fire) 5 30))
  (doseq [meteor (:meteors state)]
    (q/image (q/load-image "images/meteor.png")
             (:x meteor)
             (:y meteor)))
  (q/fill 255 255 255)
  (q/text-size 20)
  (q/text (str (:score state)) 10 30))

; update method
(defn update-state [state]
 (-> state
  (update-in  [:fires] (fn [fires] (doall (map move-fire fires))))
  (update-in  [:meteors] (fn [meteors] (doall (map move-meteors meteors))))
  (update-in [:rocket] move-rocket)
  (update-in [:fires] remove-fire)
  meteor-out
  create-meteor
  meteor-hit
  reset-game))
  ;fire-hit))


;; old move methods
(defn move1 [state event]
  (case (:key event)
    (:w :up) (update-in state [:rocket :y] - 10)
    (:s :down) (update-in state [:rocket :y] + 10)
    (:a :left) (update-in state [:rocket :x] - 10)
    (:d :right) (update-in state [:rocket :x] + 10)))

(defn on-key-up [state]
  (if (contains? #{:right :left}
                 (q/key-as-keyword))
    (assoc-in state [:rocket :dir] 5)
    (if (contains? #{:up :down}
                   (q/key-as-keyword))
        (assoc-in state [:rocket :dir] 6)
        state)))


;; defsketch
(q/defsketch rocket_game
  :host "host"
  :title "rocket game"
  :size [600 700]
  :setup setup
  :draw draw
  :key-pressed move
  ;:key-released on-key-up
  :update update-state
  :middleware [m/fun-mode])
