;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname week4setC) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; -----------------------------------------------------------------------------
;; constants

(define SIZE  600)
(define CLOSE (/ SIZE 100)) 
(define BACKG (empty-scene SIZE SIZE))

(define FOOD (square 10 "solid" "green"))
(define NOFOOD '())

(define TKY-RADIUS 5)
(define TKY-STEP 10)
(define PLAYER-COLOR "blue")
(define AI-COLOR     "brown")

;; -----------------------------------------------------------------------------
;; data definitions 
(define-struct gobble (time ai player food))
(define-struct turkey (size color loc goal))
(define-struct food (x y))
;; Gobbler = (make-gobble Number Turkey Turkey Food)
;; Turkey  = (make-turkey Number String Posn OptPosn)
;; Food is one of NOFOOD or (make-food x y)
;; OptPosn is one of #false or Posn
;; INTERPRETATION (make-gobble t ai-turkey player-turkey f) represents a
;;  game state with remaining clock ticks t, the two turkeys, and food f.
;;  NOFOOD as food means one of the turkeys has eaten the food.
;;  (make-turkey s c loc g) is a turkey of size s, color c, currently
;;  located at loc and with goal g.
;;  #false as a goal means the turkey has no goal and sits still, 

(define random-posn (make-posn (random SIZE) (random SIZE)))
(define t-player (make-turkey TKY-RADIUS PLAYER-COLOR random-posn #false))
(define t-ai (make-turkey TKY-RADIUS AI-COLOR (make-posn 1 1) (make-posn 2 2)))
(define food1 (make-food 10 20))
(define sample-world (make-gobble 10 t-ai t-player food1))

(define t-player2 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 95 95) (make-posn 96 96)))
(define t-ai2 (make-turkey TKY-RADIUS AI-COLOR random-posn #false)) ; AI Turkey with no goal.
(define t-player3 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 100 100) (make-posn 50 50)))
(define t-ai3 (make-turkey TKY-RADIUS AI-COLOR (make-posn 1 1) (make-posn 50 50)))


(define random-posn4 (make-posn (random SIZE) (random SIZE)))
(define t-player4 (make-turkey TKY-RADIUS PLAYER-COLOR random-posn4 #false))
(define t-ai4 (make-turkey TKY-RADIUS AI-COLOR (make-posn 1 1) (make-posn 2 2)))
(define food4 NOFOOD)
(define sample-world4 (make-gobble 5 t-ai4 t-player4 food4))

(define t-player5 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 10 10) #false))
(define t-ai5 (make-turkey TKY-RADIUS AI-COLOR (make-posn 5 5) (make-posn 7 7)))
(define food5 (make-food 13 7))
(define sample-world5 (make-gobble 0 t-ai5 t-player5 food5))

(define t-player6 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 10 10) (make-posn 20 20)))
(define t-ai6 (make-turkey TKY-RADIUS AI-COLOR (make-posn 5 5) (make-posn 7 7)))
(define food6 (make-food 12 8))
(define sample-world6 (make-gobble 5 t-ai6 t-player6 food6))

(define t-player7 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 10 10) (make-posn 15 15)))
(define t-ai7 (make-turkey TKY-RADIUS AI-COLOR (make-posn 5 5) (make-posn 7 7)))
(define food7 (make-food 3 6))
(define sample-world7 (make-gobble 5 t-ai7 t-player7 food7))

;; -----------------------------------------------------------------------------
;; PositiveNumber -> String
;; produce "you" if the player ate the food,
;; "ai" if the auto player ate it,
;; "tie" if the clock expired 
(define (main time)
  (winner 
   (big-bang (create-world time)
             (to-draw   render-world)
             (on-tick   move-turkeys)
             (on-mouse  update-player-goal)
             (stop-when no-time-no-food?))))

;; -----------------------------------------------------------------------------
;; PositiveNumber -> Gobbler
;; create a random world from the given amount of time

;; this test was formulated _after_ the fact to check for typos
;; it also demostrates that it is acceptable to use helper functions in tests 
(check-random
 (create-world 10)
 (make-gobble
  10
  (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))
  (create-turkey PLAYER-COLOR #false)
  (make-food (random SIZE) (random SIZE))))

(define (create-world t)
  (make-gobble
   t
   (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))
   (create-turkey PLAYER-COLOR #false)
   (make-food (random SIZE) (random SIZE))))

;; String String OptPosn -> Turkey
;; create a randomly placed turkey from the given name, color, and goal posn

(check-random
 (create-turkey "b" #false)
 (make-turkey TKY-RADIUS "b" (make-posn (random SIZE) (random SIZE)) #false))

(define (create-turkey color goal)
  (make-turkey TKY-RADIUS color (make-posn (random SIZE) (random SIZE)) goal))

;; -----------------------------------------------------------------------------

;; Gobbler -> String
;; determines who (if anyone) won the game
(check-expect (winner (make-gobble 0 t-ai t-player NOFOOD)) "tie")
(check-expect (winner (make-gobble 5 t-ai
                                   (make-turkey
                                    (add1 TKY-RADIUS)
                                    PLAYER-COLOR
                                    random-posn
                                    random-posn)
                                   NOFOOD))
              "you")
(check-expect (winner (make-gobble 0 (make-turkey
                                      (add1 TKY-RADIUS)
                                      AI-COLOR
                                      random-posn
                                      random-posn)
                                   t-player
                                   NOFOOD))
              "ai")             
(define (winner g)
  (cond
    [(> (turkey-size (gobble-player g))
        (turkey-size (gobble-ai g)))
     "you"]
    [(< (turkey-size (gobble-player g))
        (turkey-size (gobble-ai g)))
     "ai"]
    [else "tie"]))

;; *** place your wish list here *** 

;; Gobbler -> Image
;; renders the game state as an image
#|
(check-expect (render-world sample-world)
              (place-images
               (list (circle 5 "solid" "brown")
                     (circle 5 "solid" "blue")
                     (square 10 "solid" "green"))
               (list (make-posn 1 1)
                     random-posn
                     (make-posn 10 20))
               BACKG))
(check-expect (render-world sample-world4)
              (place-images
               (list (circle 5 "solid" "brown")
                     (circle 5 "solid" "blue"))
               (list (make-posn 1 1)
                     random-posn4)
               BACKG))
|#
(define (render-world g)
  (render-turkeys (gobble-ai g) (gobble-player g) (render-food (gobble-food g) BACKG)))

;; ListTurkey Turkey Image -> Image
;; adds turkeys to given image       
(check-expect (render-turkeys (cons t-ai '()) t-player2 BACKG)
              (place-images
               (list
                (circle TKY-RADIUS "solid" AI-COLOR)
                (circle TKY-RADIUS "solid" PLAYER-COLOR))
               (list
                (make-posn 1 1)
                (make-posn 95 95))
               BACKG))
(check-expect (render-turkeys
               (cons t-ai
                     (cons
                      (make-turkey TKY-RADIUS AI-COLOR (make-posn 120 120) #false) '()))
               t-player2 BACKG)
              (place-images
               (list
                (circle TKY-RADIUS "solid" AI-COLOR)
                (circle TKY-RADIUS "solid" AI-COLOR)
                (circle TKY-RADIUS "solid" PLAYER-COLOR))
               (list
                (make-posn 1 1)
                (make-posn 120 120)
                (make-posn 95 95))
               BACKG))         
(define (render-turkeys lt tp im)
  (cond
    [(empty? (rest lt))
     (render-turkey (first lt)
                    (render-turkey tp im))]
    [(cons? (rest lt))
     (render-turkeys (rest lt) tp (render-turkey (first lt) im)) ]))

;; Turkey Image -> Image
;; adds a turkey to the given image
(check-expect (render-turkey t-ai BACKG)
              (place-image (circle TKY-RADIUS "solid" AI-COLOR)
                           1 1 BACKG))
(check-expect (render-turkey t-player2 BACKG)
              (place-image (circle TKY-RADIUS "solid" PLAYER-COLOR)
                           95 95 BACKG))
(define (render-turkey t im)
  (place-image
   (circle (turkey-size t) "solid" (turkey-color t))
   (posn-x (turkey-loc t)) (posn-y (turkey-loc t))
   im))

;; Food Image -> Image
;; adds food (if any) to given image
(check-expect (render-food food5 BACKG)
              (place-image FOOD 13 7 BACKG))
(check-expect (render-food NOFOOD BACKG)
              BACKG)

(define (render-food f im)
  (cond
    [(empty? f) im]
    [else (place-image FOOD (food-x f) (food-y f) im)]))

;; Gobbler -> Gobbler
;; moves the turkeys towards their goal
;; and decrements the tick countdown
;; If turkey arrives at food, eats it.
;; If AI turkey reaches goal, but finds no food, gets a new goal
;; If AI turkey reaches goal, and finds food, gets a new goal as well.

(check-random (move-turkeys sample-world)
              (make-gobble
               9
               (make-turkey TKY-RADIUS AI-COLOR
                            (move-toward (turkey-loc t-ai) (turkey-goal t-ai) TKY-STEP)
                            (make-posn (random SIZE) (random SIZE)))
               t-player
               food1))

(check-random (move-turkeys (make-gobble
                             10
                             t-ai
                             t-player
                             (make-food 2 2)))
              (make-gobble
               9
               (make-turkey (+ 1 TKY-RADIUS) AI-COLOR
                            (move-toward (turkey-loc t-ai) (turkey-goal t-ai) TKY-STEP)
                            (make-posn (random SIZE) (random SIZE)))
               t-player
               NOFOOD))

(check-expect (move-turkeys (make-gobble
                             10
                             t-ai2
                             t-player2
                             food1))
              (make-gobble
               9
               t-ai2
               (make-turkey TKY-RADIUS PLAYER-COLOR
                            (move-toward (turkey-loc t-player2) (turkey-goal t-player2) TKY-STEP)
                            (turkey-goal t-player2))
               food1))

(check-expect (move-turkeys (make-gobble
                             10
                             t-ai2
                             t-player
                             food1))
              (make-gobble
               9
               t-ai2
               t-player
               food1))
#|
(check-within (move-turkeys (make-gobble
                             10
                             t-ai3
                             t-player3
                             food1))
              (make-gobble
               9
               (make-turkey TKY-RADIUS AI-COLOR
                            (make-posn 5 5)
                            (turkey-goal t-ai3))
               (make-turkey TKY-RADIUS PLAYER-COLOR
                            (make-posn 95 95)
                            (turkey-goal t-player3))
               food1)
              5)
|#
(define (move-turkeys g)
  (eats-food?
   (count-down
    (move-ai-and-player g))))

;; Gobble -> Gobble
;; moves both ai and player
(check-random (move-ai-and-player sample-world)
              (make-gobble 10
                           (make-turkey TKY-RADIUS
                                        AI-COLOR
                                        (make-posn 2 2)
                                        (make-posn (random SIZE) (random SIZE)))
                           t-player
                           food1))
(define (move-ai-and-player g)
  (make-gobble (gobble-time g)
               (move-ai (gobble-ai g))
               (move-player (gobble-player g))
               (gobble-food g)))

;; Turkey -> Turkey
;; moves player turkey in the direction of its goal if there is one
(check-expect (move-player t-player2)
              (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 96 96) (make-posn 96 96)))
(check-within (move-player t-player3)
              (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 95 95) (make-posn 50 50)) 5)
(check-expect (move-player t-player4)
              t-player4)
(define (move-player t)
  (cond
    [(boolean? (turkey-goal t)) t]
    [else
     (make-turkey (turkey-size t)
                  (turkey-color t)
                  (move-toward (turkey-loc t)
                               (turkey-goal t)
                               TKY-STEP)
                  (turkey-goal t))
     ]))

;; Turkey -> Turkey
;; moves ai turkey in the direction of its goal if there is one
;; and gives it a new random goal.
(check-expect (move-ai t-ai2)
              t-ai2)
(check-random (turkey-goal (move-ai t-ai3))
              (turkey-goal (make-turkey
                            TKY-RADIUS
                            AI-COLOR
                            (make-posn 6 6)
                            (make-posn (random SIZE) (random SIZE)))))
(check-within (turkey-loc (move-ai t-ai3))
              (make-posn 8 8)
              1)
(check-random (move-ai t-ai4)
              (make-turkey
               TKY-RADIUS
               AI-COLOR
               (make-posn 2 2)
               (make-posn (random SIZE) (random SIZE))))
(define (move-ai t)
  (cond
    [(boolean? (turkey-goal t)) t]
    [else
     (make-turkey (turkey-size t)
                  (turkey-color t)
                  (move-toward (turkey-loc t)
                               (turkey-goal t)
                               TKY-STEP)
                  (make-posn (random SIZE) (random SIZE)))]))

;; Gobble -> Gobble
;; Updates food field of given Gobble to NOFOOD if any turkey is close enought to eat it
;; Else return without any change to the food field
(check-expect (eats-food? sample-world6)
              (make-gobble 5
                           t-ai6
                           (make-turkey 6 PLAYER-COLOR (make-posn 10 10) (make-posn 20 20))
                           NOFOOD))
(check-expect (eats-food? sample-world7)
              (make-gobble 5
                           (make-turkey 6 AI-COLOR (make-posn 5 5) (make-posn 7 7))
                           t-player7
                           NOFOOD))
(check-expect (eats-food? sample-world)
              (make-gobble 10
                           t-ai
                           t-player
                           food1))
(define (eats-food? g)
  (cond
    [(turkey-close-enough?
      (gobble-food g)
      (gobble-ai g))
     (make-gobble (gobble-time g)
                  (fatten-turkey (gobble-ai g))
                  (gobble-player g)
                  NOFOOD)]
    [(turkey-close-enough?
      (gobble-food g)
      (gobble-player g))
     (make-gobble (gobble-time g)
                  (gobble-ai g)
                  (fatten-turkey (gobble-player g))
                  NOFOOD)]
    [else g]))

;; Food Turkey -> Boolean
;; determines if the given turkey is close enough to eat the food
(check-expect (turkey-close-enough? food6 t-player6) #t)
(check-expect (turkey-close-enough? food6 t-ai6) #f)
(check-expect (turkey-close-enough? food7 t-player7) #f)
(check-expect (turkey-close-enough? food7 t-ai7) #t)
(define (turkey-close-enough? f t)
  (close? (make-posn (food-x f)
                     (food-y f))
          (turkey-loc t)
          CLOSE))

;; Turkey -> Turkey
;; increments the radius of the given turkey by one.
(check-expect (fatten-turkey t-player6)
              (make-turkey (add1 TKY-RADIUS) PLAYER-COLOR (make-posn 10 10) (make-posn 20 20)))
(check-expect (fatten-turkey (make-turkey 200 AI-COLOR (make-posn 50 50) (make-posn 75 75)))
              (make-turkey 201 AI-COLOR (make-posn 50 50) (make-posn 75 75)))
(define (fatten-turkey t)
  (make-turkey (add1 (turkey-size t))
               (turkey-color t)
               (turkey-loc t)
               (turkey-goal t)))                     

;; Gobble -> Gobble
;; decrement the time left by one second
(check-expect (count-down sample-world)
              (make-gobble 9 t-ai t-player food1))
(define (count-down g)
  (make-gobble (sub1 (gobble-time g))
               (gobble-ai g)
               (gobble-player g)
               (gobble-food g)))


;; Gobbler MouseX MouseY MouseEvent -> Gobbler
;; Updates Player turkey's OptPosn to where the mouse click is

(check-expect (update-player-goal sample-world 20 30 "button-down")
              (make-gobble 10
                           (make-turkey TKY-RADIUS AI-COLOR (make-posn 1 1) (make-posn 2 2))
                           (make-turkey TKY-RADIUS PLAYER-COLOR random-posn (make-posn 20 30))
                           (make-food 10 20)))

(check-expect (update-player-goal sample-world 20 30 "enter")
              sample-world)

(define (update-player-goal g x y me)
  (if (string=? me "button-down")
      (make-gobble (gobble-time g)
                   (gobble-ai g)
                   (make-turkey (turkey-size (gobble-player g))
                                (turkey-color (gobble-player g))
                                (turkey-loc (gobble-player g))
                                (make-posn x y))
                   (gobble-food g))
      g))

;; Gobbler -> Boolean
;; Determines whether the game has ended
;; The game will end when one of two condition is reached:
;; - The time ran out
;; - There is no more food

(check-expect (no-time-no-food? sample-world) #f)
(check-expect (no-time-no-food? sample-world4) #t)
(check-expect (no-time-no-food? sample-world5) #t) 

(define (no-time-no-food? g)
  (or (= (gobble-time g) 0) (empty? (gobble-food g))))

;; -----------------------------------------------------------------------------
;; two helper functions that rely on domain knowledge from geometry

;; Posn Posn -> Number
;; compute a Posn that is by delta closer to q than p
;; unless p is alreay delta-close to q
(check-within (move-toward (make-posn 12 5) (make-posn 24 10) 13)
              (make-posn 24 10)
              .1)
(check-within (move-toward (make-posn 12 5) (make-posn 24 10) 6.5)
              (make-posn 18 7.5)
              .1)
(define (move-toward p q delta)
  (cond
    [(close? p q delta) q]
    [else (posn+ p (posn* (/ delta (size (posn- q p))) (posn- q p)))]))

;; Posn Posn Number -> Boolean
;; is the distance between p and q strictly less than delta (delta-close)
(check-expect (close? (make-posn 10 10) (make-posn 10 9) 2.0) #true)
(check-expect (close? (make-posn 10 10) (make-posn 10 9) 0.8) #false)
(define (close? p q delta)
  (< (distance p q) delta))

;; -----------------------------------------------------------------------------
;; a library of positions (Cartesian points) and vectors (steps between points)

;; Vec is Posn.
;; INTEREPRATION When we call a Posn a Vec, we think of it 'directionally'
;; as in x over on the horizontal and y over on the verticla axis from here 

;; Posn Posn -> Number
;; compute the distance between p and q
(check-expect (distance (make-posn 3 4) (make-posn 0 0)) 5)
(define (distance p q)
  (size (posn- p q)))

;; Vec -> Number 
;; determine the size (length) of p
(check-expect (size (make-posn 12 5)) 13)
(define (size p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; Number Vec -> Vec
;; multiply s componentwise with v
(check-expect (posn* 2 (make-posn 1 3)) (make-posn 2 6))
(define (posn* s v)
  (make-posn (* s (posn-x v)) (* s (posn-y v))))

;; Posn Posn -> Vec
;; subtract q from p componentwise to obtain a vector
(check-expect (posn- (make-posn 3 2) (make-posn 3 8)) (make-posn 0 -6))
(define (posn- p q)
  (make-posn (- (posn-x p) (posn-x q)) (- (posn-y p) (posn-y q))))

;; Posn Vec -> Posn
;; add q to p componentwise
(check-expect (posn+ (make-posn 3 2) (make-posn 3 8)) (make-posn 6 10))
(define (posn+ p q)
  (make-posn (+ (posn-x p) (posn-x q)) (+ (posn-y p) (posn-y q))))