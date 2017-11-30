;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 9a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Kevin Zhang and Celine Yan
; Week 9 Exercise A


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
(define-struct food  (x y))
;; Gobbler = (make-world Number [Listof Turkey] Turkey [Listof Food])
;; Turkey  = (make-turkey Number String Posn OptPosn)
;; Food is a (make-food x y)
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
(define t-ai2 (make-turkey TKY-RADIUS AI-COLOR (make-posn 20 20) (make-posn 15 6)))
(define food1 `(,(make-food 10 20)))
(define food2 `(,(make-food 10 20) ,(make-food 17 20)))
(define food3 `(,(make-food 50 50) ,(make-food 51 51) ,(make-food 200 164)))
(define sample-world (make-gobble 10 (list t-ai) t-player food1))
#|
;; -----------------------------------------------------------------------------
;; PositiveNumber -> String
;; produce "you" if the player ate the food,
;; "ai" if the auto player ate it,
;; "tie" if the clock expired

(define (main t)
  (winner 
   (big-bang (create-world t)
             (to-draw   render-gobbler)
             (on-tick   move-turkeys .5)
             (on-mouse  place-player-goal)
             (stop-when game-over? render-gobbler))))
|#

;; -----------------------------------------------------------------------------
;; PositiveNumber -> Gobbler
;; create a random world from the given amount of time

;; this test was formulated _after_ the fact to check for typos
;; it also demostrates that it is acceptable to use helper functions in tests 
(check-random
 (create-world 10)
 (make-gobble
  10
  (list (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))
        (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))
        (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE))))
  (create-turkey PLAYER-COLOR #false)
  (make-food (random SIZE) (random SIZE))))

(define (create-world t)
  (make-gobble
   t
   (list (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))
         (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE)))
         (create-turkey AI-COLOR (make-posn (random SIZE) (random SIZE))))
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

;;==========================================================================================
;;                          n e w  |  t e s t  | c a s e s 
;;==========================================================================================
;; GOBBLER TEST CASES
;; -----------------------------------
;; sample-world-a:
;; - a list of 2 ai turkeys
;; no more food
(define sample-world-a (make-gobble 4
                                    (list t-ai t-ai2)
                                    (make-turkey 1 PLAYER-COLOR (make-posn 4 5) #false)
                                    NOFOOD))
;; sample-world-b:
;; - a list of 1 ai turkey
;; no more food
(define sample-world-b (make-gobble 4
                                    (list t-ai)
                                    (make-turkey 1 PLAYER-COLOR (make-posn 4 5) #false)
                                    NOFOOD))
;; sample-world-c:
;; - a list of 1 ai turkey
;; - 10 ticks, and everything else is same as sample-world-d
;; 
(define sample-world-c (make-gobble 10
                                    (list t-ai)
                                    (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 4 5) #false)
                                    food1))
;; sample-world-d:
;; - a list of 1 ai turkey
;; - 9 ticks, and everything else is same as sample-world-c
(define sample-world-d (make-gobble 9
                                    (list (make-turkey TKY-RADIUS
                                                       AI-COLOR
                                                       (make-posn 2 2)
                                                       (make-posn 2 2)))
                                    (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 4 5) #false)
                                    food1))

;; sample-world-e:
;; - a list of 1 ai turkey
;; ai eats food (ai radius * 2)
(define sample-world-e (make-gobble 10
                                    (list (make-turkey (* TKY-RADIUS 2) AI-COLOR (make-posn 10 20)
                                                       (make-posn 10 20)))
                                    (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 4 5)
                                                 (make-posn 10 20))
                                    food1))
;; sample-world-f:
;; - a list of 1 ai turkey
;; player eats food (player radius * 2)
(define sample-world-f (make-gobble 10
                                    (list (make-turkey TKY-RADIUS AI-COLOR (make-posn 4 5)
                                                       (make-posn 10 20)))
                                    (make-turkey (* TKY-RADIUS 2) PLAYER-COLOR (make-posn 10 20)
                                                 (make-posn 10 20))
                                    food1))

;; sample-world-g
;; - a list of 1 ai turkey
;; time is up
(define sample-world-g (make-gobble 0
                                    (list t-ai)
                                    (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 4 5) #false)
                                    food1))

;; sample-world-h
;; - a list of 1 turkey
(define sample-world-h (make-gobble 10
                                    (list t-ai)
                                    (make-turkey TKY-RADIUS
                                                 PLAYER-COLOR
                                                 (make-posn 4 5)
                                                 (make-posn 3 4))
                                    food1))

;; sample-world-i
;; - a list of 2 turkeys
;; there is food
(define sample-world-i (make-gobble 4
                                    (list t-ai t-ai2)
                                    (make-turkey 1 PLAYER-COLOR (make-posn 4 5) #false)
                                    food1))

;; [Listof Turkey] TEST CASES
;; -----------------------------------
;; sample-lt-a
(define sample-lt-a (list t-ai t-ai2))

;; sample-lt-b
(define sample-lt-b (list t-ai))

#|

;;==========================================================================================
;;                                   w i n n e r  
;;==========================================================================================
;; Gobbler -> String
;; Produces the result of which player won the game, one of:
;; - (1) "you" if you the player ate the food, checking the size of turkey
;; - (2) "ai" if the auto player ate the food, checking the size of turkey
;; - (3) "tie" if the time is up
(check-expect (winner sample-world-f) "you")
(check-expect (winner sample-world-e)  "ai")
(check-expect (winner sample-world-g) "tie")

(define (winner g)
  (cond
    [(< (return-turkey-size (gobble-player g)) (max-size (gobble-ai g))) "ai"]
    [(> (return-turkey-size (gobble-player g)) (max-size (gobble-ai g))) "you"]
    [else "tie"]))

;;                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                          ~~~~~~~~~~ HELPER FUNCTION ~~~~~~~~~~~~~
;;                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [Listof Turkey] -> Number
;; outputs the largest size of a Turkey in the [Listof Turkey]
(check-expect (max-size (list (make-turkey TKY-RADIUS AI-COLOR (make-posn 4 5) (make-posn 10 20))))
              TKY-RADIUS)
(check-expect (max-size (list (make-turkey TKY-RADIUS AI-COLOR (make-posn 4 5) (make-posn 10 20))
                              (make-turkey 35 AI-COLOR (make-posn 4 5) (make-posn 10 20))))
              35)

;; [Listof Turkey] -> Number
;; outputs the largest size of a turkey in the given list
(define (max-size lot)
  (local (;; X = Turkey
          ;; Turkey Turkey -> Boolean
          ;; compares two Turkey sizes
          (define (size-bigger t1 t2)
            (> (return-turkey-size t1) (return-turkey-size t2))))
    (return-turkey-size (first (sort lot size-bigger)))))


;;                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                          ~~~~~~~~~~ HELPER FUNCTION ~~~~~~~~~~~~~ 
;;                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Turkey -> Number
;; Returns the size of the turkey
(check-expect (return-turkey-size t-ai) 5)
(check-expect (return-turkey-size (make-turkey 23 "red" (make-posn 5 5) (make-posn 5 5))) 23)

(define (return-turkey-size t)
  (turkey-size t))


;;==========================================================================================
;;                           r e n d e r - g o b b l e r   
;;==========================================================================================
;; Gobbler -> Image
;; Depicts the worldstate of the game
(check-expect (render-gobbler sample-world-i)
              (parse-thru-list sample-lt-a
                               (render-turkey (make-turkey 1 PLAYER-COLOR (make-posn 4 5) #false)
                                              (render-food food1 BACKG))))

(check-expect (render-gobbler 
               (make-gobble 10
                            (list (make-turkey TKY-RADIUS AI-COLOR (make-posn 3 1) (make-posn 2 2)))
                            (make-turkey TKY-RADIUS AI-COLOR (make-posn 2 1) (make-posn 2 2))
                            (make-food 10 10)))
              (render-turkey (make-turkey TKY-RADIUS AI-COLOR
                                          (make-posn 3 1)
                                          (make-posn 2 2))
                             (render-turkey (make-turkey TKY-RADIUS AI-COLOR
                                                         (make-posn 2 1)
                                                         (make-posn 2 2))
                                            (render-food (make-food 10 10)
                                                         BACKG))))
 
(define (render-gobbler g)
  (parse-thru-list (gobble-ai g)
                   (render-turkey (gobble-player g)
                                  (render-food (gobble-food g)
                                               BACKG))))

;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                         ~~~~~~~~~~ HELPER FUNCTION ~~~~~~~~~~~~~  
;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [Listof Food] Image -> Image
;; Places given food or NOFOOD onto the given image
(check-expect (render-food NOFOOD BACKG) BACKG)
(check-expect (render-food food1 BACKG) (place-image FOOD 10 20 BACKG))
(check-expect (render-food (make-food 20 30) BACKG) (place-image FOOD 20 30 BACKG))
 
(define (render-food f img)
  (cond
    [(empty? f) img]
    [else (place-image FOOD (food-x f) (food-y f) img)]))

;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                         ~~~~~~~~~~ HELPER FUNCTION ~~~~~~~~~~~~~
;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [Listof Turkey] Image -> Image
;; Recursively feeds turkeys in list of ai turkeys into render-turkey
(check-expect (parse-thru-list sample-lt-a BACKG)
              (place-image (circle TKY-RADIUS "solid" AI-COLOR)
                           1
                           1
                           (place-image (circle TKY-RADIUS "solid" AI-COLOR) 20 20 BACKG)))
(check-expect (parse-thru-list sample-lt-b BACKG)
              (place-image (circle TKY-RADIUS "solid" AI-COLOR)
                           1
                           1
                           BACKG))

;; [List-of Turkey] Image -> Image
;; recursively feeds turkeys in list of ai turkeys into render-turkey

(define (parse-thru-list lot img)
    (foldl render-turkey img lot)) 

;;                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                          ~~~~~~~~~~ HELPER FUNCTION ~~~~~~~~~~~~~ 
;;                          ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Turkey Image -> Image
;; Places given turkey onto the given image
(check-expect (render-turkey t-ai BACKG) (place-image (circle TKY-RADIUS "solid" AI-COLOR) 1 1 BACKG))
(check-expect (render-turkey (make-turkey TKY-RADIUS AI-COLOR (make-posn 2 3) (make-posn 2 2)) BACKG)
              (place-image (circle TKY-RADIUS "solid" AI-COLOR) 2 3 BACKG))

(define (render-turkey t img)
  (place-image (circle (turkey-size t) "solid" (turkey-color t))
               (posn-x (turkey-loc t)) (posn-y (turkey-loc t)) img))

;;==========================================================================================
;;                                m o v e - t u r k e y s
;;==========================================================================================
;; Gobbler -> Gobbler
;; (1) Updates the time, positions, and sizes of the player and ai turkeys depending
;;     if the mouse has been clicked or not 

(check-expect (move-turkeys sample-world-c) sample-world-d) 
(check-within (move-turkeys (make-gobble 10  
                                         (list
                                          (make-turkey TKY-RADIUS ;; t-ai
                                                       AI-COLOR
                                                       (make-posn 10 20)
                                                       (make-posn 10 20))
                                          (make-turkey TKY-RADIUS ;; t-ai2
                                                       AI-COLOR
                                                       (make-posn 25 25)
                                                       (make-posn 16 16)))
                                         (make-turkey TKY-RADIUS ;; player 
                                                      PLAYER-COLOR
                                                      (make-posn 4 5)
                                                      (make-posn 10 20))
                                         food1))
              (make-gobble 9  
                           (list
                            (make-turkey 10 ;; t-ai that is on top of the food
                                         AI-COLOR
                                         (move-toward
                                          (make-posn 10 20)
                                          (make-posn 10 20)
                                          CLOSE)
                                         (make-posn 10 20))
                            (make-turkey TKY-RADIUS ;; t-ai2
                                         AI-COLOR
                                         (move-toward
                                          (make-posn 25 25)
                                          (make-posn 16 16)
                                          CLOSE)
                                         (make-posn 16 16)))
                           (make-turkey TKY-RADIUS ;; player 
                                        PLAYER-COLOR
                                        (move-toward
                                         (make-posn 4 5)
                                         (make-posn 10 20)
                                         CLOSE)
                                        (make-posn 10 20))
                           NOFOOD) 1)

(define (move-turkeys g)
  (make-gobble (sub1 (gobble-time g))
               (update-listturkey (gobble-food g)(gobble-ai g)) 
               (update-turkey (gobble-food g)(gobble-player g))
               (update-food g))) 

;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                         ~~~~~~~~~~ HELPER FUNCTION ~~~~~~~~~~~~~ 
;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Gobble --> [Listof Food]
;; determines current Food depending whether:
;; (1) if Food is a NOFOOD, then return a NOFOOD
;; (2) if a turkey is close enough to Food, changes Food to NOFOOD
;; (3) otherwise, Food stays the same
(check-expect (update-food sample-world-a) NOFOOD)
(check-expect (update-food sample-world-f) NOFOOD)
(check-expect (update-food sample-world-d) food1)

(define (update-food g)
  (cond
    [(empty? (gobble-food g)) NOFOOD]
    [else (local (;; Posn [List-of Turkey] Number -> Boolean
                  ;; takes a list of turkeys and checks if each of them are close to the food
                  (define (listturkey-close? p lt delta)
                    (local (;; X = Turkey
                            ;; Turkey -> Boolean
                            ;; checks if the given turkey is close to the food
                            (define (turkey-close? t)
                              (close? (turkey-loc t) p delta))
                            )
                      (ormap turkey-close? lt)))
                  )
            (cond
              [(or
                (listturkey-close? (make-posn (food-x (gobble-food g)) (food-y (gobble-food g)))
                                   (gobble-ai g)
                                   CLOSE)
                (close? (make-posn (food-x (gobble-food g)) (food-y (gobble-food g)))
                        (turkey-loc (gobble-player g))
                        CLOSE))
               NOFOOD]
              [else (gobble-food g)]))]))

;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                         ~~~~~~~~~~ HELPER FUNCTION ~~~~~~~~~~~~~
;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [Listof Food] [Listof Turkey] -> [Listof Turkey]
;; Contructs a [Listof Turkey] in which each Turkey has its updated size and position
(check-expect (update-listturkey food1 sample-lt-b) (list (update-turkey food1 t-ai)))
(check-within (update-listturkey food1 sample-lt-a) (list (update-turkey food1 t-ai)
                                                          (update-turkey food1 t-ai2)) 1)

;; [Listof Food] [List-of Turkey] -> [List-of Turkey]
;; constructs a [List-of Turkey] in which each Turkey has its updated size and position

(define (update-listturkey f lot)
  (local (;; X = Turkey, Y = Turkey
          ;; Turkey -> Turkey
          ;; updates the turkey size and position
          (define (local-update-turkey t)
            (update-turkey f t)))
    (map local-update-turkey lot))) 

;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                         ~~~~~~~~~~ HELPER FUNCTION ~~~~~~~~~~~~~
;;                         ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
;; [Listof Food] Turkey -> Turkey
;; Constructs a turkey with its updated size and updated position,
;; however, if the player's turkey has not set its goal point, then it will not move
(check-expect (update-turkey food1 (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 5 5) #false))
              (make-turkey TKY-RADIUS PLAYER-COLOR (make-posn 5 5) #false))
(check-within (update-turkey (make-food 3 3)
                             (make-turkey TKY-RADIUS
                                          AI-COLOR
                                          (make-posn 50 50)
                                          (make-posn 3 3)))
              (make-turkey TKY-RADIUS
                           AI-COLOR
                           (move-toward (make-posn 50 50) (make-posn 3 3) CLOSE)
                           (make-posn 3 3))
              1)

(define (update-turkey f t)
  (cond
    [(boolean? (turkey-goal t)) t]
    [else (local
            (;; [Listof Food] Turkey -> Number
             ;; Returns the turkey's size if a turkey is close enough to the food to eat it.
             (define (grow-turkey f t)
               (cond
                 [(empty? f) (turkey-size t)]
                 [else (cond
                         [(close? (make-posn (food-x f) (food-y f))
                                  (turkey-loc t) CLOSE) (* 2 (turkey-size t))]
                         [else (turkey-size t)])])))
            (make-turkey (grow-turkey f t)
                         (turkey-color t)
                         (move-toward (turkey-loc t) (turkey-goal t) CLOSE)
                         (turkey-goal t)))]))

;;==========================================================================================
;;                       p l a c e - p l a y e r - g o a l 
;;==========================================================================================
;; Gobbler Number Number MouseEvent -> Gobbler
;; Updates the player's goal when the mouse is clicked (if me is "button-down")
(check-expect (place-player-goal sample-world-c 5 5 "button-down")
              (make-gobble 10 (list t-ai) (make-turkey TKY-RADIUS
                                                       PLAYER-COLOR
                                                       (make-posn 4 5)
                                                       (make-posn 5 5))
                           food1))
(check-expect (place-player-goal sample-world-e 5 5 "leave") sample-world-e)

(define (place-player-goal g x y me)
  (cond
    [(string=? me "button-down")
     (local (;; Turkey Number Number -> Turkey
             ;; Sets the player's goal at the x and y
             (define (update-goal t x y)
               (make-turkey (turkey-size t) (turkey-color t) (turkey-loc t) (make-posn x y))))
       (make-gobble (gobble-time g)
                    (gobble-ai g)
                    (update-goal (gobble-player g) x y)
                    (gobble-food g)))]
    [else g]))

;;==========================================================================================
;;                              g a m e - o v e r ?  
;;==========================================================================================
;; Gobbler -> Boolean 
;; Determines if the game is over when:
;; - (1) The number of ticks has reached 0 (true)
;; - (2) All of the food have been eaten   (true)
;; - (3) None of the above                 (false)
(check-expect (game-over? sample-world-g) #true)
(check-expect (game-over? sample-world-b) #true)
(check-expect (game-over? sample-world-c) #false)

(define (game-over? g)
  (local
    (;; Gobbler -> Boolean
     ;; Determines if the number of ticks is 0
     (define (time-is-up? g)
       (zero? (gobble-time g)))
     ;; Gobbler -> Boolean
     ;; Determines if there is no more food left
     (define (no-more-food? g)
       (empty? (gobble-food g))))
    (cond
      [(time-is-up? g) #true]
      [(no-more-food? g) #true]
      [else #false])))
|#

;;==========================================================================================
;;                              E X E R C I S E 3 
;;==========================================================================================

;; [Listof Turkey] [Listof Food] -> [Listof Turkey]
;; fattens turkeys near each piece of food once they eat it
(check-expect (eat* sample-lt-b food1) sample-lt-b)

(check-expect (eat* sample-lt-a food2)
              (list t-ai
                    (make-turkey (* 2 (turkey-size t-ai2))
                                 (turkey-color t-ai2)
                                 (turkey-loc t-ai2)
                                 (turkey-goal t-ai2))))

(check-expect (eat* sample-lt-a food3) sample-lt-a)
(define (eat* lot lof)
  (local (; Turkey -> Turkey
          ; fattens the given turkey
          (define (fatten-turkey t)
            (make-turkey (* 2 (turkey-size t))
                         (turkey-color t)
                         (turkey-loc t)
                         (turkey-goal t)))
          ; Turkey Food -> Boolean
          ; Determines if the turkey is close to the given food
          (define (turkey-close? t f)
            (close? (make-posn (food-x f) (food-y f))
                    (turkey-loc t)
                    CLOSE))
          ; Food [Listof Turkey]-> [Listof Turkey]
          ; Fattens the first turkey that is close to the food.
          (define (first-come-first-serve f lot)
            (cond
              [(empty? lot) lot]
              [else (if (turkey-close? (first lot) f)
                        (cons (fatten-turkey (first lot)) (rest lot))
                        (cons (first lot) (first-come-first-serve f (rest lot))))]))
           ) 
  (foldr first-come-first-serve lot lof)))

; [Listof Food] [Listof Turkey] -> [Listof Food]
; Removes each piece of food that could be eaten
(check-expect (was-eaten* food1 sample-lt-a) food1)
(check-expect (was-eaten* food2 sample-lt-a) food1)

(define (was-eaten* lof lot)
  (local (; Food -> Boolean
          ; determines if the piece of food was eaten by any turkeys
          (define (food-eaten? f)
            (local ( (define food-posn (make-posn (food-x f) (food-y f)))
                     )
              (not
               (ormap (lambda (t) (close? food-posn (turkey-loc t) CLOSE)) lot))))
          )
    (filter food-eaten? lof)))


;; -----------------------------------------------------------------------------
;; two helper functions that rely on domain knowledge from geometry

;; REVISED SIGNATURE 
;; Posn Posn Number -> Posn 
;; compute a Posn that is by delta closer to q than p
;; unless p is alreay delta-close to q

(check-within (move-toward (make-posn 12 5) (make-posn 24 10) 13)
              (make-posn 24 10)
              .1)
(check-within (move-toward (make-posn 12 5) (make-posn 24 10) 6.5)
              (make-posn 18 7.5)
              .1)

;; NEW TEST ;; MODIFIED October 2, per request by Antone on behalf of the class
(check-within (move-toward (make-posn 12 5) (make-posn 24 10) 14)
              (make-posn 24 10)
              .1)

(define (move-toward origin destination delta)
  (cond
    [(close? origin destination delta) destination]
    [else
     (posn+ origin (posn* (/ delta (size (posn- destination origin))) (posn- destination origin)))]))

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