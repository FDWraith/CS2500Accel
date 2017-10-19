;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab6b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Exercise 4 - Bubble Simulator

;; simulates blowing bubbles:
;; -- their birth (on mouse click)
;; -- their growth (on clock tick), 

;; --
;; ABSTRACTIONS

;; [List-of Bubble] -> Image
;; Draws all the bubbles onto the background
(define (render-bubbles lb)
  (local (;; Bubble -> Image
          ;; draws a bubble
          (define (bubble-image b)
            (circle (bubble-size b) 'outline (bubble-color b)))
          ;; X = Bubble, Y = Image
          ;; Bubble Image -> Image
          ;; places bubble onto image
          (define (draw-bubble b img)
            (local ((define loc (bubble-loc b))
                    (define l.x (posn-x loc))
                    (define l.y (posn-y loc)))
              (place-image (bubble-image b) l.x l.y img)))
          )
    (foldr draw-bubble BACKGROUND lb)))


;; [List-of Bubble] -> [List-of Bubble]
;; grows the young bubbles
;; pops old bubbles
;; moves all bubbles according to their velocity,

(define (update-bubbles lb)
  (move-bubbles (aging (birthday lb))))

(check-expect (update-bubbles empty) empty)
(check-expect (update-bubbles (list OLD)) empty)
(check-expect (update-bubbles BUBBLE*-LIST1) BUBBLE*-up)



;; -----------------------------------------------------------------------------
;; CONSTANTS
(define WIDTH 700)
(define HEIGHT 700)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define DEATH-AGE 300)
(define GROWTH-AGE 6)
(define MAX-SIZE 100)
(define MAX-SPEED 10)
(define GROWTH-FACTOR 10)

;; -----------------------------------------------------------------------------
;; the state of the bubble simulation

(define-struct bubble [size loc vel age color])
;; A Bubble is a (make-bubble Number Posn Posn Number String)
;; INTERPRETATION
;; - size is the radius of the bubble
;; - loc is the current position of the center of the bubble
;; - vel is the velocity of the bubble
;; - age is the number of seconds since the bubble's creation
;; - color is the bubble color

;; Bubble* is a [List-of Bubble]

(define BUBBLE1 (make-bubble 30 (make-posn 100 100) (make-posn 2 10) 0 'blue))
(define 1-moved (make-bubble 30 (make-posn 102 110) (make-posn 2 10) 0 'blue))
(define 1-grown (make-bubble 40 (make-posn 100 100) (make-posn 2 10) 0 'blue))
(define 1-up    (make-bubble 40 (make-posn 102 110) (make-posn 2 10) 1 'blue))

(define BUBBLE2 (make-bubble 10 (make-posn 200 100) (make-posn 4 8) 0 'pink))
(define 2-moved (make-bubble 10 (make-posn 204 108) (make-posn 4 8) 0 'pink))
(define 2-grown (make-bubble 20 (make-posn 200 100) (make-posn 4 8) 0 'pink))
(define 2-aged  (make-bubble 10 (make-posn 200 100) (make-posn 4 8) 1 'pink))
(define 2-up    (make-bubble 20 (make-posn 204 108) (make-posn 4 8) 1 'pink))

(define BUBBLE3 (make-bubble 80 (make-posn 670 10) (make-posn -3 5) 2 'purple))
(define 3-moved (make-bubble 80 (make-posn 667 15) (make-posn -3 5) 2 'purple))
(define 3-grown (make-bubble 90 (make-posn 670 10) (make-posn -3 5) 2 'purple))
(define 3-aged  (make-bubble 80 (make-posn 670 10) (make-posn -3 5) 3 'purple))
(define 3-up    (make-bubble 90 (make-posn 667 15) (make-posn -3 5) 3 'purple))

(define BUBBLE4 (make-bubble 45 (make-posn 340 230) (make-posn 5 -5) 0 'yellow))
(define 4-moved (make-bubble 45 (make-posn 345 225) (make-posn 5 -5) 0 'yellow))
(define 4-grown (make-bubble 55 (make-posn 340 230) (make-posn 5 -5) 0 'yellow))
(define 4-up    (make-bubble 55 (make-posn 345 225) (make-posn 5 -5) 1 'yellow))

(define BUBBLE*-LIST1 (list BUBBLE1 BUBBLE2 BUBBLE3 BUBBLE4))
(define BUBBLE*-MOVED (list 1-moved 2-moved 3-moved 4-moved))
(define BUBBLE*-GROWN (list 1-grown 2-grown 3-grown 4-grown))
(define BUBBLE*-up    (list 1-up    2-up    3-up    4-up))

(define OLD  (make-bubble 10 (make-posn 200 100) (make-posn 4 8) 301 'green))
(define AGED (make-bubble 10 (make-posn 200 100) (make-posn 4 8) 302 'green))

;; -----------------------------------------------------------------------------
;; N -> Bubble*
;; Simulate the given number of bubbles, click to add a new bubble
(define (main n)
  (big-bang (create-bubble* n)
    [to-draw  render-bubbles]
    [on-tick  update-bubbles]
    [on-mouse new-bubble]))

;; -----------------------------------------------------------------------------


;; [List-of Bubble] -> Image
;; Draw all the bubbles onto the background

(check-expect (render-bubbles empty) BACKGROUND)
(check-expect (render-bubbles (list BUBBLE1 BUBBLE2))
              (local ((define back  BACKGROUND)
                      (define pink  (circle 10 'outline 'pink))
                      (define +pink (place-image pink 200 100 back))
                      (define blue  (circle 30 'outline 'blue))
                      (define +blue (place-image blue 100 100 +pink)))
                +blue))
#|
(define (render-bubbles lb)
  (cond [(empty? lb) BACKGROUND]
        [else (draw-bubble (first lb) (render-bubbles (rest lb)))]))

;; Bubble Image -> Image
;; draws the bubble on top of a given image
(check-expect (draw-bubble BUBBLE2 BACKGROUND)
              (place-image (bubble-image BUBBLE2) 200 100 BACKGROUND))
(define (draw-bubble b img)
  (local ((define loc (bubble-loc b))
          (define l.x (posn-x loc))
          (define l.y (posn-y loc)))
    (place-image (bubble-image b) l.x l.y img)))

;; Bubble -> Image
;; renders the image of a bubble
(check-expect (bubble-image BUBBLE4) (circle 45 'outline 'yellow))
(define (bubble-image b)
  (circle (bubble-size b) 'outline (bubble-color b)))
|#

;; -----------------------------------------------------------------------------
;; [List-of Bubble] -> [List-of Bubble]
;; grows the young bubbles
;; pops old bubbles
;; moves all bubbles according to their velocity,

(define (update-bubbles lb)
  (move-bubbles (aging (birthday lb))))

(check-expect (update-bubbles empty) empty)
(check-expect (update-bubbles (list OLD)) empty)
(check-expect (update-bubbles BUBBLE*-LIST1) BUBBLE*-up)

;; -----------------------------------------------------------------------------
;; [List-of Bubble] -> [List-of Bubble]
;; updates age of bubbles

(check-expect (birthday empty) empty)
(check-expect (birthday (list OLD BUBBLE3)) (list AGED 3-aged))

(define (birthday lb)
  (cond [(empty? lb) '()]
        [else (cons (add-age (first lb)) (birthday (rest lb)))]))

;; Bubble -> Bubble
;; increases the age of a bubble
(check-expect (add-age BUBBLE2) 2-aged)
(define (add-age b)
  (make-bubble (bubble-size b)
               (bubble-loc b)
               (bubble-vel b)
               (add1 (bubble-age b))
               (bubble-color b)))

;; -----------------------------------------------------------------------------
;; [List-of Bubble] -> [List-of Bubble]
;; old bubbles pop and young bubbles grow

(define TOO-OLD-TO-GROW
  (make-bubble 31 (make-posn 100 100) (make-posn 2 10) 10 'blue))

(check-expect (aging empty) empty)
(check-expect (aging (list OLD TOO-OLD-TO-GROW)) (list TOO-OLD-TO-GROW))
(check-expect (aging BUBBLE*-LIST1) BUBBLE*-GROWN)

(define (aging lb)
  (grow-the-young (pop lb)))

;; [List-of Bubble] -> [List-of Bubble]
;; pops bubbles that are older than an age threshold
(check-expect (pop (list BUBBLE1 BUBBLE2)) (list BUBBLE1 BUBBLE2))
(check-expect (pop (list OLD)) '())
(define (pop lb)
  (cond [(empty? lb) '()]
        [else (if (not-old? (first lb))
                  (cons (first lb) (pop (rest lb)))
                  (pop (rest lb)))]))

;; Bubble -> Boolean
;; determines whether a bubble is still under the age threshold
(check-expect (not-old? BUBBLE1) #true)
(check-expect (not-old? OLD) #false)
(define (not-old? b)
  (< (bubble-age b) DEATH-AGE))


;; [List-of Bubble] -> [List-of Bubble]
;; increases size of all bubbles in the list younger than a threshold
(check-expect (grow-the-young '()) '())
(check-expect (grow-the-young (list BUBBLE2 BUBBLE3)) (list 2-grown 3-grown))
(check-expect (grow-the-young (list OLD)) (list OLD))
(define (grow-the-young.v0 lb)
  (cond [(empty? lb) empty]
        [else (if (young? (first lb))
                  (cons (grow-bubble (first lb)) (grow-the-young (rest lb)))
                  (cons (first lb) (grow-the-young (rest lb))))]))

;; [List-of Bubble] -> [List-of Bubble]
;; increases size of all bubbles in the list younger than a threshold
(define (grow-the-young lb)
  (local (
          )
    ()))


;; Bubble -> Boolean
;; is a bubble young enough to grow?
(check-expect (young? BUBBLE4) #true)
(check-expect (young? OLD) #false)
(define (young? b)
  (< (bubble-age b) GROWTH-AGE))

;; Bubble -> Bubble
;; increases size of given bubble
(check-expect (grow-bubble BUBBLE3) 3-grown)
(define (grow-bubble b)
  (make-bubble (+ (bubble-size b) GROWTH-FACTOR)
               (bubble-loc b)
               (bubble-vel b)
               (bubble-age b)
               (bubble-color b)))
        
;; -----------------------------------------------------------------------------
;; [List-of Bubble] -> [List-of Bubble]
;; moves bubbles according to their velocity
(check-expect (move-bubbles empty) empty)
(check-expect (move-bubbles BUBBLE*-LIST1) BUBBLE*-MOVED)
(define (move-bubbles lb)
  (cond [(empty? lb) empty]
        [else (cons (move (first lb)) (move-bubbles (rest lb)))]))
                    
;; Bubble -> Bubble
;; moves a bubble according to its velocity
(check-expect (move BUBBLE4) 4-moved)
(define (move b)
  (make-bubble (bubble-size b)
               (update-loc (bubble-loc b) (bubble-vel b))
               (bubble-vel b)
               (bubble-age b)
               (bubble-color b)))

;; -----------------------------------------------------------------------------
;; [List-of Bubble] Number Number MouseEvent -> [List-of Bubble]
;; adds a bubble at a random location
(check-expect (new-bubble BUBBLE*-LIST1 30 200 "drag")  BUBBLE*-LIST1)
(check-random (new-bubble empty 100 100 "button-down")
              (list (create-a-bubble 456)))
(define (new-bubble lb x y me)
  (if (string=? me "button-down")  (cons (create-a-bubble 123) lb) lb))

;; -----------------------------------------------------------------------------
;; N -> [List-of Bubbles]
;; generates the given number of random bubbles
(check-expect (create-bubble* 0) empty)
(check-random (create-bubble* 2)
              (list (create-a-bubble 1) (create-a-bubble 2)))

(define (create-bubble* n)
  (cond [(zero? n) empty]
        [else (cons (create-a-bubble n)(create-bubble* (sub1 n)))]))

;; -----------------------------------------------------------------------------
;; LIBRARY

;; Posn Posn -> Posn
;; updates the given location using a velocity posn
(check-expect (update-loc (make-posn 10 40) (make-posn 2 -3))
              (make-posn 12 37))
(define (update-loc p vel)
  (make-posn (modulo (+ (posn-x p) (posn-x vel)) WIDTH)
             (modulo (+ (posn-y p) (posn-y vel)) HEIGHT)))

;; Any -> Bubble
;; create a random bubble 
(define (create-a-bubble _)
  (make-bubble (random MAX-SIZE)
               (make-posn (random WIDTH) (random HEIGHT))
               (make-posn (- (random (* MAX-SPEED 2)) MAX-SPEED)
                          (- (random (* MAX-SPEED 2)) MAX-SPEED)) 
               0 (random-color (random MAX-SIZE))))

;; Number -> Symbol
(define (random-color num)
  (if (odd? num) 'pink 'blue))