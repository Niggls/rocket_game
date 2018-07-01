(ns rocket-game.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))


(defn star-color-index [x]
  (if (< x 12)
      0
      (if (< x 15)
          2
          3)))

(defn create-star [y]
 {:x (rand-int (q/width))
   :y (rand-int y)
   :size (+ (rand-int 5) 1)
   :speed (+ (rand-int 3) 1)
   :color (star-color-index (rand-int 20))})

;; function for reset state
(defn reset-state-variable [state]
 {:rocket {:image (q/load-image "images/rocket.png")
           :x 260
           :y 340
           :dir 0}
  :background (q/load-image "images/stars.jpg")
  :fires []
  :smoke []
  :score 0
  :stars (:stars state)
  :highscore (if ( > (:score state) (:highscore state))
                 (:score state)
                 (:highscore state))
  :gameOver true
  :meteors []
  :bonus []})

;; setup: here we define our global state variable
;; # --> anonymous function
(defn setup []
  ;; these two lines, a map (data structure) is added in step 1-6
  {:rocket {:image (q/load-image "images/rocket.png")
            :x 260
            :y 340
            :dir -1}
   :background (q/load-image "images/stars.jpg")
   :fires []
   :score 0
   :smoke []
   :highscore 0
   :stars (take 150 (repeatedly #(create-star (q/height))))
   :gameOver false
   :meteors []
   :bonus []})

;;;; helper methods;;;;;;;;;;;;;;;;;;;;;;;;
(defn inside? [x y]
  (or
   (< x -12)
   (> (+ x 33) (q/width))
   (< y 0)
   (> (+ y 40) (q/height))))

(defn item-inside? [item]
 (let [x (:x item)
       y (:y item)]
  (> y (q/height))))

(defn remove-stars [stars]
 (remove item-inside? stars))

(defn meteor-out [state]
  (let [old (-> state :meteors (count))
        new-meteor (remove item-inside? (:meteors state))
        new (count new-meteor)]
   {:rocket (:rocket state)
    :background (q/load-image "images/stars.jpg")
    :fires (:fires state)
    :score (+ (:score state) (- old new))
    :highscore (:highscore state)
    :gameOver false
    :smoke (:smoke state)
    :stars (:stars state)
    :meteors new-meteor
    :bonus (:bonus state)}))

(defn meteor-hit [state]
 (let [rocket-x (-> state :rocket :x)
       rocket-y (-> state :rocket :y)
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
           (reset-state-variable state)
           state))))

(defn bonus-out [state]
  (if (item-inside? (:bonus state))
    state
    {:rocket (:rocket state)
     :background (q/load-image "images/stars.jpg")
     :fires (:fires state)
     :score (:score state)
     :highscore (:highscore state)
     :gameOver false
     :smoke (:smoke state)
     :stars (:stars state)
     :meteors (:meteors state)
     :bonus []}))

(defn bonus-hit [state]
 (let [rocket-x (-> state :rocket :x)
       rocket-y (-> state :rocket :y)
       bonus (get (:bonus state) 0)]
   (if (empty? bonus)
       state
       (if (or (and
                 (<= (:x bonus) rocket-x (+ (:x bonus) 40))
                 (<= (:y bonus) rocket-y (+ (:y bonus) 40)))
               (and
                 (<= (:x bonus) rocket-x (+ (:x bonus) 40))
                 (<= (:y bonus) (+ rocket-y 45) (+ (:y bonus) 40)))
               (and
                 (<= (:x bonus) (+ rocket-x 45) (+ (:x bonus) 40))
                 (<= (:y bonus) rocket-y (+ (:y bonus) 40)))
               (and
                 (<= (:x bonus) (+ rocket-x 45) (+ (:x bonus) 40))
                 (<= (:y bonus) (+ rocket-y 45) (+ (:y bonus) 40))))
         {:rocket (:rocket state)
          :background (q/load-image "images/stars.jpg")
          :fires (:fires state)
          :score (+ (:score state) (:points bonus))
          :highscore (:highscore state)
          :gameOver false
          :smoke (:smoke state)
          :stars (:stars state)
          :meteors (:meteors state)
          :bonus []}
         state))))

;; # defines a function --> (fn [oldAge] (+ oldAge 0.3))
(defn age-smoke [smoke]
  (update-in smoke [:age] #(+ % 0.3)))

(defn old? [smoke]
  (< 2.0 (:age smoke)))

(defn remove-old-smokes [smokes]
  (remove old? smokes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; creation methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn create-meteor [state]
 (if (= (rand-int 10) 1)
     (if-not (or
               (-> state :rocket :dir (= 0))
               (-> state :rocket :dir (= -1)))
             (update-in state [:meteors] conj {:x (rand-int (+ (q/width) -40)) :y -40 :speed (+ (rand-int 30) 5)})
             ;(update-in state [:meteors] conj {:x (rand-int (+ (q/width) -40)) :y -40 :speed 1})
             state)
     state))

(defn create-smoke [x y]
  {:pos [(+ x 25 (- (rand-int 10) 5))
         (+ y 50 (- (rand-int 10) 5))]
   :dir 0.0
   :age 0.0
   :col [(+ (rand-int 105) 150)
         (+ (rand-int 100) 100)
         (rand-int 100)]})

(defn emit-smoke [state]
  (let [x (-> state :rocket :x)
        y (-> state :rocket :y)]
    (update-in state [:smoke] conj (create-smoke x y))))

(defn create-new-star [state]
 (if(= (rand-int 7) 1)
   (if-not (or
             (-> state :rocket :dir (= 0))
             (-> state :rocket :dir (= -1)))
           {:rocket (:rocket state)
            :background (q/load-image "images/stars.jpg")
            :fires (:fires state)
            :score (:score state)
            :highscore (:highscore state)
            :gameOver true
            :smoke (:smoke state)
            :stars (conj (:stars state) (create-star 1))
            :meteors (:meteors state)
            :bonus (:bonus state)}
           state)
   state))

(defn create-bonus [state]
 (if (and (empty? (:bonus state)) (= (rand-int 100) 1))
   (if-not (or
             (-> state :rocket :dir (= 0))
             (-> state :rocket :dir (= -1)))
           (if (= (rand-int 5) 1)
             (update-in state [:bonus] conj {:x (rand-int (+ (q/width) -40)) :y (rand-int (+ (q/height) -40)) :points 25 :speed 3 :image "images/bonus2.png"})
             (update-in state [:bonus] conj {:x (rand-int (+ (q/width) -40)) :y (rand-int (+ (q/height) -40)) :points 10 :speed 2 :image "images/bonus.png"}))
     state)
  state))

(defn fly-backwards [smoke state]
 (if (-> state :rocket :dir (= 2))
     []
     smoke))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; reset methods;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset-game [state]
  (if
    (inside? (:x (:rocket state )) (:y (:rocket state)))
    (reset-state-variable state)
    state))

(defn reset-game-over [gameOver state]
  (if (-> state :rocket :dir (not= 0))
    false
    true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

;;;;;;;; move methods;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move [state event]
  (case (:key event)
    (:w :up) (assoc-in state [:rocket :dir] 1)
    (:s :down) (assoc-in state [:rocket :dir] 2)
    (:a :left) (assoc-in state [:rocket :dir] 3)
    (:d :right) (assoc-in state [:rocket :dir] 4)
    state))

(defn move-meteors [meteor]
 (let [speed (:speed meteor)]
  (update-in meteor [:y] #(+ % speed))))

(defn move-star [star]
 (update-in star [:y] #(+ % (:speed star))))

(defn move-stars [state]
 (if-not (or
           (= (:dir (:rocket state)) 0)
           (= (:dir (:rocket state)) -1))
         {:rocket (:rocket state)
          :background (q/load-image "images/stars.jpg")
          :fires (:fires state)
          :score (:score state)
          :highscore (:highscore state)
          :gameOver true
          :smoke (:smoke state)
          :stars (doall (map move-star (:stars state)))
          :meteors (:meteors state)
          :bonus (:bonus state)}
         state))

(defn move-bonus [state]
  (if (empty? (:bonus state))
    state
    (update-in (:bonus state) [:y] + 4)))

(defn move-rocket [rocket]
 (case (:dir rocket)
  (1) (update-in  rocket [:y] - 10)
  (2) (update-in  rocket [:y] + 10)
  (3) (update-in  rocket [:x] - 10)
  (4) (update-in  rocket [:x] + 10)
  (0) (update-in  rocket [:x] + 0)
  (-1) (update-in  rocket [:x] + 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; draw method
(defn draw [state]
  ;; q/background-image and q/image functions are added in step 1-6
  ;(q/background-image (:background state))
  (q/background 0)
  (q/fill 250 250 250)
  (q/stroke 250 250 250)
  (doseq [star (:stars state)]
   (if (= (:color star) 0)
       (do
         (q/fill 250 250 250)
         (q/stroke 250 250 250)
         (q/ellipse (:x star) (:y star) (:size star) (:size star)))
       (if (= (:color star) 1)
           (do
             (q/fill 255 255 26)
             (q/stroke 255 255 26)
             (q/ellipse (:x star) (:y star) (:size star) (:size star)))
           (do
             (q/fill 255 77 77)
             (q/stroke 255 77 77)
             (q/ellipse (:x star) (:y star) (:size star) (:size star))))))
  (doseq [bonus (:bonus state)]
    (q/image (q/load-image (:image bonus))
             (:x bonus)
             (:y bonus)
             45 45))
  (q/image (:image (:rocket state))
           (:x (:rocket state))
           (:y (:rocket state)))
  (q/fill 0 0 255)
  (q/text-align :left)
  (q/stroke 0 0 255)
  (doseq [meteor (:meteors state)]
    (q/image (q/load-image "images/meteor.png")
             (:x meteor)
             (:y meteor)))
  (doseq [smoke (:smoke state)]
    (let [age (:age smoke)
          size (max 0.0 (- 10.0 (* 5.0 age)))
          [r g b] (:col smoke)
          [x y] (:pos smoke)]
      (q/fill 0 0 250 150)
      (q/stroke 0 0 250 150)
      (q/ellipse x y size size)))
  (q/fill 255 255 255)
  (q/text-size 20)
  (q/text (str "Score: " (:score state)) 10 30)
  (q/text (str "Highscore: " (:highscore state)) (- (q/width) 140) 30)
  (q/fill 200 0 0)
  (q/text-font (q/create-font "DejaVu Sans" 40 true))
  (q/text-align :center)
  (when (:gameOver state)
    (q/text (str "Game Over...\nMove to try again") (/ (q/width) 2) 500)))

; update method
(defn update-state [state]
 (-> state
  (update-in [:meteors] (fn [meteors] (doall (map move-meteors meteors))))
  (update-in [:rocket] move-rocket)
  ; (update-in [:bonus] (fn [bonus] (doall (map move-bonus bonus))))
  move-stars
  create-new-star
  (update-in [:stars] remove-stars)
  emit-smoke
  (update-in [:smoke] (fn [smokes] (map age-smoke smokes)))
  (update-in [:smoke] remove-old-smokes)
  meteor-out
  create-meteor
  ; bonus-out
  create-bonus
  bonus-hit
  meteor-hit
  reset-game
  (update-in [:smoke] fly-backwards state)
  (update-in [:gameOver] reset-game-over state)))

;; defsketch
(q/defsketch rocket_game
  :host "host"
  :title "rocket game"
  :size [600 700]
  :setup setup
  :draw draw
  :key-pressed move
  :update update-state
  :middleware [m/fun-mode])
