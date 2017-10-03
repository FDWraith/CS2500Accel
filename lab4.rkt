;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
;; Constants
(define SPEED 10)
(define BACKG-SIZE 600)
(define BALLOON-RAD 20)
(define BALLOON-COLOR "green")
(define BALLOON (circle BALLOON-RAD "solid" BALLOON-COLOR))
(define BACKG (empty-scene BACKG-SIZE BACKG-SIZE))

(define-struct balloon (loc color))
;; A Balloon is a (make-balloon Posn ColorString)
;; represents the location and color of the balloon

;; A LOB(list of balloons) is one of:
;; (cons Balloon '())
;; (cons Balloon LOB)

(define-struct game-state (remaining popped))
;; A GameState(GS) is a (make-game-state LOB N)
;; INTEPRETATION remaining is the list of balloons not popped or out of screen
;; and popped is the number of balloons popped

;; N -> N
;; runs the balloon-popping game with given number of balloons
;; returns number of balloons popped
(define (main n)
  (game-state-popped
   (big-bang (make-game-state (n-balloons n) 0)
             [to-draw render]
             [on-tick move-balloons]
             [on-mouse pop-balloon]
             [stop-when screen-empty?])))

;; N -> LOB
;; generates a LOB with randomly located balloons
(define (n-balloons n)
  (cons (make-balloon (make-posn 0 0)) '()))

;;
;; GS -> Image
;; renders the game state as an image
(define (render gs) BACKG)

;; GS MouseX MouseY MouseEvent -> GS
;; pops the balloon at the mouse location (if any)
(define (pop-balloon gs x y me) gs)

;; GS -> Boolean
;; determines if the game ends
;; ends when there is no more balloon on the screen
(define (screen-empty? gs) #f)
|#

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
(require 2htdp/image)
(require 2htdp/universe)

;; -----------------------------------------------------------------------------
(define WIDTH          500)
(define HEIGHT         500)
(define BALLOON-SPEED  1)
(define BALLOON-RADIUS 10)

(define BACKGROUND     (empty-scene WIDTH HEIGHT))
(define GREEN          (circle BALLOON-RADIUS 'solid 'green))

;; -----------------------------------------------------------------------------
(define-struct balloon [loc color])
;; A Balloon is a (make-balloon Posn Color)
;; loc: the location of the balloon
;; color: the color of the balloon

(define BALLOON0      (make-balloon (make-posn 40 60) 'green))
(define BALLOON1      (make-balloon (make-posn 200 0) 'green))

;; An LoB (List of Balloon) is one of:
;; - '()
;; - (cons Balloon LoB)

(define LOB0      '())
(define LOB1      (cons BALLOON0 LOB0))
(define LOB2      (cons BALLOON1 LOB1))

(define-struct world [balloons spawned])
;; A BalloonWorld is a (make-balloon-world LoB N)
;; - balloons: the balloons currently in flight
;; - spawned: the number of balloons at the start of the game
(define BW0      (make-world LOB0 0))
(define BW1      (make-world LOB2 4))

;; -----------------------------------------------------------------------------
;; N -> N
;; Run the balloon game with the given number of balloons and determine how
;; the player poppped.
(define (main num-balloons)
  (count-popped
   (big-bang (initial-world num-balloons)
             [on-tick next-world]
             [to-draw render]
             [on-mouse mouse-handler]
             [stop-when game-over?])))

;; BalloonWorld -> Number
;; Get the number of balloons that were popped during the game
(check-expect (count-popped BW0) 0)
(check-expect (count-popped BW1) 2)
(define (count-popped w)
  (- (world-spawned w)
     (length (world-balloons w))))

;; =============================================================================
;;             HERE IS THE WISH LIST
;; =============================================================================

;; BalloonWorld -> BalloonWorld
;; Produce the next world by moving all the balloons 

(define BALLOON0-NEXT (make-balloon (make-posn 40 (- 60 BALLOON-SPEED)) 'green))
(define BALLOON1-NEXT (make-balloon (make-posn 200 (- 0 BALLOON-SPEED)) 'green))
(define LOB0-NEXT '())
(define LOB1-NEXT (cons BALLOON0-NEXT LOB0-NEXT))
(define LOB2-NEXT (cons BALLOON1-NEXT LOB1-NEXT))
(define BW0-NEXT (make-world LOB0-NEXT 0))
(define BW1-NEXT (make-world LOB2-NEXT 4))

(check-expect (next-world BW0) BW0-NEXT)
(check-expect (next-world BW1) BW1-NEXT)

(define (next-world w)
  (make-world (move-balloons (world-balloons w))
              (world-spawned w)))

;; LOB -> LOB
;; moves each balloon by BALLOON-SPEED
(define (move-balloons lob)
  (cond
    [(empty? lob) lob]
    [else (cons (move-balloon (first lob))
                (move-balloons (rest lob)))]))

;; Balloon -> Balloon
;; moves a balloon by BALLOON-SPEED
(define (move-balloon b)
  (make-balloon (make-posn (posn-x (balloon-loc b))
                           (- (posn-y (balloon-loc b)) BALLOON-SPEED))
                (balloon-color b)))

;; -----------------------------------------------------------------------------
;; BalloonWorld Number Number MouseEvent -> BalloonWorld
;; Pop a balloon if the user clicked on one
(check-expect (mouse-handler BW0 40 60 "button-down") BW0)
(check-expect (mouse-handler BW1 40 60 "button-down")
              (make-world (cons BALLOON1 LOB0) 4))
(check-expect (mouse-handler BW1 40 60 "button-up") BW1)

(define (mouse-handler w x y me)
  (cond
    [(string=? me "button-down") (make-world (pop-balloons x y (world-balloons w))
                                             (world-spawned w))]
    [else w]))

;; N N LOB -> LOB
;; Pops a balloon (if any) at the given location
(check-expect (pop-balloons 40 60 LOB0) LOB0)
(check-expect (pop-balloons 40 60 LOB2) (cons BALLOON1 LOB0))
(define (pop-balloons x y l)
  (cond
    [(empty? l) l]
    [else (cond
            [(pop-balloon? x y (first l)) (pop-balloons x y (rest l))]
            [else (cons (first l) (pop-balloons x y (rest l)))])]))

;; N N Balloon -> Boolean
;; determines if the balloon is popped with the given popping coordinates
(check-expect (pop-balloon? 40 60 BALLOON1) #f)
(check-expect (pop-balloon? 40 60 BALLOON0) #t)
(define (pop-balloon? x y b)
  (< (distance-between (make-posn x y) (balloon-loc b)) BALLOON-RADIUS))

;; -----------------------------------------------------------------------------
;; BalloonWorld -> Boolean
;; Have all the balloons either been popped or escaped?
(check-expect (game-over? BW0) #t)
(check-expect (game-over? BW1) #f)
(define (game-over? w)
  (cond
    [(empty? (world-balloons w)) #t]
    [else
     (balloons-safe? (world-balloons w))]))

;; LOB -> Boolean
;; checks if all the balloons in the given LOB are safe (out of screen)
(check-expect (balloons-safe? LOB1) #f)
(check-expect (balloons-safe? (cons (make-balloon (make-posn 100 -10) "red") '())) #t)
(check-expect (balloons-safe? (cons (make-balloon (make-posn 100 -8) "red") '())) #f)
(define (balloons-safe? lob)
  (cond
    [(empty? lob) #t]
    [else
     (and (balloon-safe? (first lob))
          (balloons-safe? (rest lob)))]))

;; Balloon -> Boolean
;; determines if the given balloon is safe (out of screen)
(check-expect (balloon-safe? BALLOON0) #f)
(check-expect (balloon-safe? (make-balloon (make-posn 100 -10) "red")) #t)
(check-expect (balloon-safe? (make-balloon (make-posn 100 -8) "red")) #f)
(define (balloon-safe? b)
  (<= (posn-y (balloon-loc b)) (* -1 BALLOON-RADIUS)))
;; -----------------------------------------------------------------------------
;; You may find this function useful. It would usually come with a library.

;; distance-between: Posn Posn -> Number
;; Calculate the distance between the two Posns

(check-expect (distance-between (make-posn 4 5) (make-posn 4 5)) 0)
(check-expect (distance-between (make-posn 0 0) (make-posn 3 4)) 5)

(define (distance-between p1 p2)
  (sqrt (+ (sqr (- (posn-x p2) (posn-x p1)))
           (sqr (- (posn-y p2) (posn-y p1))))))

;; =============================================================================
;;              END OF WISH LIST
;; =============================================================================

;; These functions render the world as an image

(define BW1-IMG (place-image GREEN 40 60 (place-image GREEN 200 0 BACKGROUND)))

;; BalloonWorld -> Scene
;; Render the given world

(check-expect (render BW0) BACKGROUND)
(check-expect (render BW1) BW1-IMG)

(define (render w)
  (render-balloons (world-balloons w) BACKGROUND))

;; LoB Scene -> Scene
;; Render all the balloons onto the scene

(check-expect (render-balloons LOB0 BACKGROUND) BACKGROUND)
(check-expect (render-balloons LOB2 BACKGROUND) BW1-IMG)

(define (render-balloons b* scene)
  (cond
    [(empty? b*) scene]
    [else (render-balloon (first b*) (render-balloons (rest b*) scene))]))

;; Balloon Scene -> Scene
;; Render the balloon onto the scene

(define BALLOON0-SCENE (place-image GREEN 40 60 BACKGROUND))
(check-expect (render-balloon BALLOON0 BACKGROUND) BALLOON0-SCENE)

(define (render-balloon balloon scene)
  (render-at (circle BALLOON-RADIUS 'solid (balloon-color balloon))
             (balloon-loc balloon)
             scene))

;; Image Posn Scene -> Scene
;; Render the image onto the scene at the given location
(check-expect (render-at (square 5 'solid 'red) (make-posn 14 25) BACKGROUND)
              (place-image (square 5 'solid 'red) 14 25 BACKGROUND))

(define (render-at img loc scene)
  (place-image img
               (posn-x loc)
               (posn-y loc)
               scene))

;; -----------------------------------------------------------------------------
;; These functions are used to produce the initial random world

;; N -> BalloonWorld
;; Produce an initial world with the given number of balloons
(check-random (initial-world 0) (make-world '() 0))
(check-random (initial-world 2)
              (make-world
               (cons (make-balloon
                      (make-posn (random WIDTH) (random HEIGHT))
                      'green)
                     (cons (make-balloon
                            (make-posn (random WIDTH) (random HEIGHT))
                            'green)
                           '()))
               2))

(define (initial-world num-balloons)
  (make-world (random-balloons num-balloons) num-balloons))

;; N -> LoB
;; Build a list of random balloon locations
(check-random (random-balloons 0) '())
(check-random (random-balloons 2)
              (cons (make-balloon
                     (make-posn (random WIDTH) (random HEIGHT))
                     'green)
                    (cons (make-balloon
                           (make-posn (random WIDTH) (random HEIGHT))
                           'green)
                          '())))
               
(define (random-balloons num-balloons)
  (cond
    [(= num-balloons 0) '()]
    [else (cons (make-balloon (make-posn (random WIDTH) (random HEIGHT)) 'green)
                (random-balloons (- num-balloons 1)))]))