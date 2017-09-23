;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname week4setA) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ------ requirements ------ ;

(require 2htdp/image)
(require 2htdp/universe)

; ------ constants ------ ;

(define SIZE  600)
(define CLOSE (/ SIZE 100)) 
(define BACKG (empty-scene SIZE SIZE))

(define FOOD (square 10 "solid" "green"))
(define NOFOOD '())

(define TKY-RADIUS 5)
(define TKY-STEP 10)
(define PLAYER-COLOR "blue")
(define AI-COLOR     "brown")

; ------ definitions ------ ;

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
(define t-player2 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 95 95) (make-posn 96 96)))
(define t-ai2 (create-turkey AI-COLOR #false)) ; AI Turkey with no goal.
(define t-ai3 (make-turkey TKY-RADIUS AI-COLOR (make-posn 1 1) (make-posn 50 50)))
(define t-player3 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 100 100) (make-posn 50 50)))
(define food1 (make-food 10 20))
(define sample-world (make-gobble 10 t-ai t-player food1))

;; -----------------------------------------------------------------------------
;; PositiveNumber -> String
;; produce "you" if the player ate the food,
;; "ai" if the auto player ate it,
;; "tie" if the clock expired 
(define (main time)
  (winner 
   (big-bang (create-world time)
     (to-draw         render-world)
     (on-tick         move-turkeys)
     (on-mouse  update-player-goal)
     (stop-when   no-time-no-food?))))

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
(define (winner g)
  "tie")

;; Gobbler -> Image
;; renders the game state as an image
(define (render-world g)
  BACKG)

;; Gobbler -> Gobbler
;; moves the turkeys towards their goal
;; and decrements the tick countdown
;; If turkey arrives at food, eat it.
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
               (make-turkey (add1 TKY-RADIUS) AI-COLOR
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
(check-expect (move-turkey (make-gobble
                            10
                            t-ai2
                            t-player
                            food1))
              (make-gobble
               9
               t-ai2
               t-player
               food1))
(check-expect (move-turkey (make-gobble
                            10
                            t-ai3
                            t-player3
                            food1))
              (make-gobble
               9
               (make-turkey TKY-RADIUS AI-COLOR
                            (move-toward (turkey-loc t-ai3) (turkey-goal t-ai3) TKY-STEP)
                            (turkey-goal t-ai3))
               (make-turkey TKY-RADIUS PLAYER-COLOR
                            (move-toward (turkey-loc t-player3) (turkey-goal t-player3) TKY-STEP)
                            (turkey-goal t-player3))))
(define (move-turkeys g)
  g)

;; Gobbler MouseEvent -> Gobbler
;; changes the player's turkey's goal according to mouse
(define (update-player-goal g me)
  g)


;; Gobbler -> Boolean
;; determines if there is no more food or if time has run out
(define (no-time-no-food? g)
  #f)

;; -----------------------------------------------------------------------------
;; two helper functions that rely on domain knowledge from geometry

;; Posn Posn Number -> Posn
;; compute a Posn that is by delta closer to q than p
;; unless p is already delta-close to q
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