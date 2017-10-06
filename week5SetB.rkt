;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname week5SetB) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;========================================== Exercise 2 ===========================================
(require 2htdp/image)
(require 2htdp/universe)

;; -----------------------------------------------------------------------------
;; constants

(define SIZE  600)
(define CLOSE (/ SIZE 100)) 
(define BACKG (empty-scene SIZE SIZE))
(define TKY-COUNT 5)
(define FOOD-COUNT (* TKY-COUNT 2))

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
;; Gobbler = (make-gobble Number ListTurkey Turkey Food)
;; A ListTurkey is one of:
;; - (cons Turkey '())
;; - (cons Turkey ListTurkey)
;; Turkey  = (make-turkey Number String Posn OptPosn)
;; Food is one of:
;; - NOFOOD
;; - (make-food x y)
;; ListFood is one of:
;; - NOFOOD
;; - (cons Food ListFood)
;; OptPosn is one of #false or Posn
;; INTERPRETATION (make-gobble t ai-turkey player-turkey f) represents a
;;  game state with remaining clock ticks t, ai turkeys and one player turkey,
;;  and pieces of food f.
;;  NOFOOD as food means one of the turkeys has eaten the food.
;;  (make-turkey s c loc g) is a turkey of size s, color c, currently
;;  located at loc and with goal g.
;;  #false as a goal means the turkey has no goal and sits still, 

(define random-posn (make-posn (random SIZE) (random SIZE)))
(define random-posn4 (make-posn (random SIZE) (random SIZE)))

(define t-player (make-turkey TKY-RADIUS PLAYER-COLOR random-posn #false))
(define t-player2 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 95 95) (make-posn 96 96)))
(define t-player3 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 100 100) (make-posn 50 50)))
(define t-player4 (make-turkey TKY-RADIUS PLAYER-COLOR random-posn4 #false))
(define t-player5 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 10 10) #false))
(define t-player6 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 10 10) (make-posn 20 20)))
(define t-player7 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 10 10) (make-posn 15 15)))

(define t-ai (make-turkey TKY-RADIUS AI-COLOR (make-posn 1 1) (make-posn 2 2)))
(define t-ai2 (make-turkey TKY-RADIUS AI-COLOR random-posn #false)) ; AI Turkey with no goal.
(define t-ai3 (make-turkey TKY-RADIUS AI-COLOR (make-posn 1 1) (make-posn 50 50)))
(define t-ai4 (make-turkey TKY-RADIUS AI-COLOR (make-posn 1 1) (make-posn 2 2)))
(define t-ai5 (make-turkey TKY-RADIUS AI-COLOR (make-posn 5 5) (make-posn 7 7)))
(define t-ai6 (make-turkey TKY-RADIUS AI-COLOR (make-posn 5 5) (make-posn 7 7)))
(define t-ai7 (make-turkey TKY-RADIUS AI-COLOR (make-posn 5 5) (make-posn 7 7)))

(define food1 (make-food 10 20))
(define food4 NOFOOD)
(define food5 (make-food 13 7))
(define food6 (make-food 12 8))
(define food7 (make-food 3 6))
(define food8 (make-food 6 6))

;; loat short for "List of AI turkeys"
(define loat1 (cons t-ai (cons t-ai3 '())))
(define loat2 (cons t-ai4 (cons t-ai5 (cons t-ai6 '()))))

;; lof short of "List of Food"
(define lof1 (list food1))
(define lof2 (list food5 food6 food7 food8))

(define sample-world (make-gobble 10 t-ai t-player food1))
(define sample-world4 (make-gobble 5 t-ai4 t-player4 food4))
(define sample-world5 (make-gobble 0 t-ai5 t-player5 food5))
(define sample-world6 (make-gobble 5 t-ai6 t-player6 food6))
(define sample-world7 (make-gobble 5 t-ai7 t-player7 food7))
(define sample-world8 (make-gobble 5 loat1 t-player6 food6)) ;;player turkey eats food
(define sample-world9 (make-gobble 5 loat2 t-player2 food8)) ;;ai turkey eats food
(define sample-world10 (make-gobble 5 loat1 t-player2 food6)) ;;no turkey eats food
(define sample-world11 (make-gobble 5 loat1 t-player2 NOFOOD))
(define sample-world12 (make-gobble 5 loat1 t-player6 lof1))
(define sample-world13 (make-gobble 5 loat1 t-player6 (list food6)))

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
  (cons (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))
        (cons (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))'()))
  (create-turkey PLAYER-COLOR #false)
  (gen-food FOOD-COUNT)))
(define (create-world t)
  (make-gobble
   t
   (cons (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))
         (cons (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))'()))
   (create-turkey PLAYER-COLOR #false)
   (gen-food FOOD-COUNT)))

;; N -> ListFood
;; randomly creates N pieces of food at random locations
(check-random (gen-food 3)
              (list (make-food (random SIZE) (random SIZE))
                    (make-food (random SIZE) (random SIZE))
                    (make-food (random SIZE) (random SIZE))))
(define (gen-food n)
  (cond
    [(= n 0) '()]
    [else (cons (make-food (random SIZE) (random SIZE)) (gen-food (- n 1)))]))

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
(check-expect (winner (make-gobble 0 (cons t-ai '()) t-player NOFOOD)) "tie")
(check-expect (winner (make-gobble 5 loat1
                                   (make-turkey
                                    (add1 TKY-RADIUS)
                                    PLAYER-COLOR
                                    random-posn
                                    random-posn)
                                   NOFOOD))
              "you")
(check-expect (winner (make-gobble 0 (cons (make-turkey
                                            (add1 TKY-RADIUS)
                                            AI-COLOR
                                            random-posn
                                            random-posn) '())
                                   t-player
                                   NOFOOD))
              "ai")

(define (winner g)
  (cond
    [(> (turkey-size (gobble-player g))
        (max-turkey-size (gobble-ai g)))
     "you"]
    [(< (turkey-size (gobble-player g))
        (max-turkey-size (gobble-ai g)))
     "ai"]
    [else "tie"]))

;; ListTurkey -> Number
;; Finds the size of the fattest turkey
(check-expect (max-turkey-size loat1) 5)
(check-expect (max-turkey-size (cons (make-turkey 100 AI-COLOR (make-posn 1 1) (make-posn 2 2))
                                     (cons t-ai '()))) 100)
(define (max-turkey-size lt)
  (cond
    [(empty? (rest lt)) (turkey-size (first lt))]
    [else (max (turkey-size (first lt)) (max-turkey-size (rest lt)))]))

;; *** place your wish list here *** 
;; Gobbler -> Image
;; renders the game state as an image

(check-expect (render-world sample-world13)
              (place-images
               (list (circle 5 "solid" "brown")
                     (circle 5 "solid" "brown")
                     (circle 5 "solid" "blue")
                     (square 10 "solid" "green"))
               (list (make-posn 1 1)
                     (make-posn 1 1)
                     (make-posn 10 10)
                     (make-posn 12 8))
               BACKG))
(check-expect (render-world sample-world11)
              (place-images
               (list (circle 5 "solid" "brown")
                     (circle 5 "solid" "brown")
                     (circle 5 "solid" "blue"))
               (list (make-posn 1 1)
                     (make-posn 1 1)
                     (make-posn 95 95))
               BACKG))

(define (render-world g)
  (render-turkeys (gobble-ai g) (gobble-player g) (render-foods (gobble-food g) BACKG)))

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

;; ListFood Image -> Image
;; adds the list of food (if any?) to given image
(check-expect (render-foods lof1 BACKG)
              (render-food food1 BACKG))
(check-expect (render-foods NOFOOD BACKG)
              BACKG)
(check-expect (render-foods lof2 BACKG)
              (render-food food5
                           (render-food food6
                                        (render-food food7
                                                     (render-food food8 BACKG)))))
(define (render-foods lof im)
  (cond
    [(empty? lof) im]
    [else
     (render-food (first lof)
                  (render-foods (rest lof) im))]))

;; Gobbler -> Gobbler
;; moves the turkeys towards their goal
;; and decrements the tick countdown
;; If turkey arrives at food, eats it.
;; If AI turkey reaches goal, but finds no food, gets a new goal
;; If AI turkey reaches goal, and finds food, gets a new goal as well.
(check-random (move-turkeys (make-gobble
                             10
                             (cons t-ai '())
                             t-player
                             lof1))
              (make-gobble
               9
               (cons (make-turkey TKY-RADIUS AI-COLOR
                                  (move-toward (turkey-loc t-ai) (turkey-goal t-ai) TKY-STEP)
                                  (make-posn (random SIZE) (random SIZE))) '())
               t-player
               lof1))

(check-random (move-turkeys (make-gobble
                             10
                             (cons t-ai '())
                             t-player
                             (list (make-food 2 2))))
              (make-gobble
               9
               (cons (make-turkey (+ 1 TKY-RADIUS) AI-COLOR
                                  (move-toward (turkey-loc t-ai) (turkey-goal t-ai) TKY-STEP)
                                  (make-posn (random SIZE) (random SIZE))) '())
               t-player
               (list NOFOOD)))

(check-expect (move-turkeys (make-gobble
                             10
                             (cons t-ai2 '())
                             t-player2
                             lof1))
              (make-gobble
               9
               (cons t-ai2 '())
               (make-turkey TKY-RADIUS PLAYER-COLOR
                            (move-toward (turkey-loc t-player2) (turkey-goal t-player2) TKY-STEP)
                            (turkey-goal t-player2))
               lof1))

(check-expect (move-turkeys (make-gobble
                             10
                             (cons t-ai2 '())
                             t-player
                             lof1))
              (make-gobble
               9
               (cons t-ai2 '())
               t-player
               lof1))
(check-random (move-turkeys (make-gobble
                             10
                             (list t-ai t-ai5)
                             t-player2
                             (list (make-food 7 7) (make-food 96 96))))
              (make-gobble
               9
               (list (make-turkey TKY-RADIUS AI-COLOR
                                  (move-toward (turkey-loc t-ai) (turkey-goal t-ai) TKY-STEP)
                                  (make-posn (random SIZE) (random SIZE)))
                     (fatten-turkey
                      (make-turkey TKY-RADIUS AI-COLOR
                                   (move-toward (turkey-loc t-ai5) (turkey-goal t-ai5) TKY-STEP)
                                   (make-posn (random SIZE) (random SIZE)))))
               (fatten-turkey
                (make-turkey TKY-RADIUS PLAYER-COLOR
                             (move-toward (turkey-loc t-player2) (turkey-goal t-player2) TKY-STEP)
                             (turkey-goal t-player2)))
               (list NOFOOD NOFOOD)))                             
(define (move-turkeys g)
  (eats-food?
   (count-down
    (move-ai-and-player g))))

;; Gobble -> Gobble
;; moves both ai and player
(check-random (move-ai-and-player
               (make-gobble 10
                            (cons t-ai '())
                            t-player
                            food1))
              (make-gobble 10
                           (cons (make-turkey TKY-RADIUS
                                              AI-COLOR
                                              (make-posn 2 2)
                                              (make-posn (random SIZE) (random SIZE))) '())
                           t-player
                           food1))
(define (move-ai-and-player g)
  (make-gobble (gobble-time g)
               (move-loat (gobble-ai g))
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

;; ListTurkey -> ListTurkey
;; moves ai turkeys in the direction of their goals if there is one
;; and gives them each a new random goal.
(check-random (move-loat (cons t-ai '()))
              (cons (move-ai t-ai) '()))
(check-random (move-loat (cons t-ai (cons t-ai2 '())))
              (cons (move-ai t-ai) (cons (move-ai t-ai2) '())))
(check-within (turkey-loc (first (move-loat (cons t-ai3 '()))))
              (make-posn 8 8)
              1)
(define (move-loat lt)
  (cond
    [(empty? (rest lt)) (cons (move-ai (first lt)) '())]
    [else (cons (move-ai (first lt)) (move-loat (rest lt))) ]))

;; Turkey -> Turkey
;; moves a single ai turkey in the direction of its goal if there is one
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
;; Updates food field of Gobble based on if any turkeys have eaten 
(check-expect (eats-food? sample-world12)
              sample-world12)
(check-expect (eats-food? (make-gobble 10
                                       (list t-ai);;at (1 1)
                                       t-player6  ;; at (10 10)
                                       (list (make-food 0 0))))
              (make-gobble 10
                           (list (fatten-turkey t-ai))
                           t-player6
                           (list NOFOOD)))
(check-expect (eats-food? (make-gobble 10
                                       (list t-ai);;at (1 1)
                                       t-player6  ;; at (10 10)
                                       (list (make-food 11 10))))
              (make-gobble 10
                           (list t-ai)
                           (fatten-turkey t-player6)
                           (list NOFOOD)))
(define (eats-food? g)
  (make-gobble
   (gobble-time g)
   (loat-eating (gobble-food g)
                (gobble-ai g))
   (turkey-eating (gobble-food g)
                  (gobble-player g))
   (foods-eaten? g)))

;; ListFood ListTurkey -> ListTurkey
;; determines if any turkey in the given list of turkey has eaten any food
;; if yes, return list of turkey with the turkey that has ate the food fattened
;; if no, return original list of turkeys without change
(check-expect (loat-eating lof1 loat1) loat1)
(check-expect (loat-eating NOFOOD loat1) loat1)
(check-expect (loat-eating (list food1 food8) loat2) (cons t-ai4 (cons (make-turkey
                                                                        (add1 TKY-RADIUS)
                                                                        AI-COLOR
                                                                        (make-posn 5 5)
                                                                        (make-posn 7 7))
                                                                       (cons
                                                                        (make-turkey
                                                                         (add1 TKY-RADIUS)
                                                                         AI-COLOR
                                                                         (make-posn 5 5)
                                                                         (make-posn 7 7))
                                                                        '()))))
(define (loat-eating lof a-loat)
  (cond
    [(empty? (rest a-loat)) (cons
                             (turkey-eating lof (first a-loat))
                             '())]
    [else
     (cons (turkey-eating lof (first a-loat))
           (loat-eating lof (rest a-loat)))]))

;; Food Turkey -> Turkey
;; determines if the given turkey has ate the food
;; if yes, fattens the turkey
;; if not, return original turkey without change
(check-expect (turkey-eating-old-ver food6 t-ai6) t-ai6)
(check-expect (turkey-eating-old-ver food7 t-ai7)
              (make-turkey (add1 TKY-RADIUS)
                           AI-COLOR
                           (make-posn 5 5)
                           (make-posn 7 7)))
(check-expect (turkey-eating-old-ver NOFOOD t-ai6) t-ai6)
(define (turkey-eating-old-ver f t)
  (cond
    [(empty? f) t]
    [else
     (cond
       [(turkey-close-enough? f t)
        (fatten-turkey t)]
       [else t])]))



;; ListFood Turkey -> Turkey
;; determines if the given turkey has ate any of the food
;; if yes, fattens the turkey
;; if not, return original turkey without change
(check-expect (turkey-eating lof1 t-ai6) t-ai6)
(check-expect (turkey-eating lof2 t-ai7)
              (make-turkey (+ 2 TKY-RADIUS)
                           AI-COLOR
                           (make-posn 5 5)
                           (make-posn 7 7)))
(check-expect (turkey-eating NOFOOD t-ai6) t-ai6)
(check-expect (turkey-eating (list NOFOOD) t-ai6) t-ai6)
(define (turkey-eating lof t)
  (cond
    [(empty? lof) t]
    [else (turkey-eating (rest lof) (turkey-eating-old-ver (first lof) t)) ]))

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

;; Gobble -> Food
;; determines if the food is eaten by any turkey
;; if yes, there is NOFOOD
;; if not, return original Food without change
(check-expect (food-eaten? sample-world8)
              NOFOOD)
(check-expect (food-eaten? sample-world9)
              NOFOOD)
(check-expect (food-eaten? sample-world10)
              (make-food 12 8))
(define (food-eaten? g)
  (cond
    [(or
      (food-eaten-by-turkeys? (gobble-food g)
                              (gobble-ai g))
      (food-eaten-by-turkey? (gobble-food g)
                             (gobble-player g)))
     NOFOOD]
    [else (gobble-food g)]))

;; Gobble -> ListFood
;; determines if any of the food in the ListFood is eaten by any turkey
;; if yes, replace the eaten food with NOFOOD
;; if no, no change
(check-expect (foods-eaten? (make-gobble 10 loat1 t-player2 lof1))
              lof1)
(check-expect (foods-eaten? (make-gobble 10 loat2 t-player2 lof2))
              (list food5 food6 NOFOOD NOFOOD))
(check-expect (foods-eaten? (make-gobble 10 loat1 t-player6 lof2))
              (list NOFOOD NOFOOD NOFOOD NOFOOD))
(define (foods-eaten? g)
  (cond
    [(empty? (gobble-food g)) (gobble-food g)]
    [else (cond
            [(food-eaten-by-turkeys? (first (gobble-food g))
                                     (cons (gobble-player g) (gobble-ai g)))
             (cons NOFOOD (foods-eaten? (make-gobble (gobble-time g)
                                                     (gobble-ai g)
                                                     (gobble-player g)
                                                     (rest (gobble-food g)))))]
            [else (cons (first (gobble-food g))
                        (foods-eaten? (make-gobble (gobble-time g)
                                                   (gobble-ai g)
                                                   (gobble-player g)
                                                   (rest (gobble-food g)))))])]))

;; Food ListTurkey -> Boolean
;; determines if the food is eaten by any turkey from the given list of turkeys
(check-expect (food-eaten-by-turkeys? food6 loat1) #f)
(check-expect (food-eaten-by-turkeys? food8 loat2) #t)
(define (food-eaten-by-turkeys? f loat)
  (cond
    [(empty? (rest loat))
     (food-eaten-by-turkey? f (first loat))]
    [else
     (or
      (food-eaten-by-turkey? f (first loat))
      (food-eaten-by-turkeys? f (rest loat)))]))

;; Food Turkey -> Boolean
;; determines if the food is eaten by the given turkey
(check-expect (food-eaten-by-turkey? food6 t-player6) #t)
(check-expect (food-eaten-by-turkey? food8 t-player2) #f)
(check-expect (food-eaten-by-turkey? NOFOOD t-player2) #f)
(define (food-eaten-by-turkey? f t)
  (cond
    [(empty? f) #f]
    [else (turkey-close-enough? f t)]))

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

;; ListFood -> Boolean
;; determines if all pieces of food in the ListFood is eaten

(check-expect (no-foods? lof1) #f)
(check-expect (no-foods? NOFOOD) #t)
(check-expect (no-foods? (list food1 food6 NOFOOD)) #f)
(check-expect (no-foods? (list NOFOOD NOFOOD NOFOOD)) #t)
(define (no-foods? lof)
  (cond
    [(empty? lof) #t]
    [else
     (and (empty? (first lof))
          (no-foods? (rest lof)))]))

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