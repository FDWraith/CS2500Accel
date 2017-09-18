;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname week2setc) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Requirements
(require 2htdp/image)
(require 2htdp/universe)
;; GAME-BACKGROUND
(define BACKGROUND-WIDTH 200)
(define BACKGROUND-HEIGHT 400)
(define GAME-BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
;; Constants
(define SIZE (/ BACKGROUND-HEIGHT 2) )

(define TANK-HEIGHT (/ SIZE 20) )
(define TANK-WIDTH (/ SIZE 10) )
(define TANK-IMAGE (rectangle TANK-WIDTH TANK-HEIGHT "solid" "blue"))

(define MISSILE-SIZE (/ TANK-WIDTH 2))
(define MISSILE-IMAGE (triangle (/ SIZE 20) "solid" "green"))
(define MISSILE-SPEED 10)

(define UFO-RADIUS TANK-WIDTH)
(define UFO-IMAGE (overlay
                   (circle UFO-RADIUS "solid" "red")
                   (rectangle (* UFO-RADIUS 2) (/ UFO-RADIUS 2) "solid" "red")))
(define UFO-SPEED 5)

;; Exercise 97

;; A NNN is a Non-Negative Number

;; A SIGS is one of
;; -- Aim
;; -- Fired
;; INTERPRETATION represents the complete state of
;; the space invader game

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
;; A Aim is (make-aim ufo tank)
;; INTERPRETATION represents the state of the game
;; when the tank is moving into poition to fire.

;; A Fired is (make-fired ufo tank missile)
;; INTERPRETATION represents the state of the game
;; when the missile has been fired.

;; A UFO is a (make-posn x y)
;; INTERPRETATION represents the UFO's location in the game.

(define-struct tank [loc vel])
;; A Tank is (make-tank loc vel)
;; loc is a NNN
;; vel is a Number
;; INTERPRETATION the tank's location is represented by (loc, BACKGROUND-HEIGHT)
;; and it moves vel units every tick.

;; A Missile is a (make-posn x y)
;; INTERPRETATION represents the Missile's position in the game.

;; Tank Image -> Image
;; generate a new image by adding the TANK-IMAGE to given image im
(check-expect (tank-render (make-tank 150 10) GAME-BACKGROUND)
              (place-image TANK-IMAGE 150 BACKGROUND-HEIGHT GAME-BACKGROUND))
(define (tank-render t im)
  (place-image TANK-IMAGE (tank-loc t) BACKGROUND-HEIGHT im))

;; UFO Image -> Image
;; generate a new image by adding the UFO-IMAGE to given image im
(check-expect (ufo-render (make-posn 150 20) GAME-BACKGROUND)
              (place-image UFO-IMAGE 150 20 GAME-BACKGROUND))
(define (ufo-render u im)
  (place-image UFO-IMAGE (posn-x u) (posn-y u) im))

;; MISSILE Image -> Image
;; generate a new image by adding the MISSILE-IMAGE to given image im
(check-expect (missile-render (make-posn 150 130) GAME-BACKGROUND)
              (place-image MISSILE-IMAGE 150 130 GAME-BACKGROUND))
(define (missile-render m im)
  (place-image MISSILE-IMAGE (posn-x m) (posn-y m) im))

#|
The two expressions will generate the same image in almost all cases,
except for in the cases when the missile and ufo are on each other, in which the
one that shows on top is dependant on which is drawn last.
|#

;; Exercise 98
;; SIGS -> Boolean
;; check if either missile hits ufo or ufo landed, which is when the game ends
(check-expect (si-game-over? (make-aim (make-posn 100 120) (make-tank 24 3))) #f)
(check-expect (si-game-over?
               (make-fired (make-posn 100 120) (make-tank 100 10) (make-posn 100 140)))
              #f)
(check-expect (si-game-over?
               (make-fired (make-posn 100 150) (make-tank 24 3) (make-posn 100 140)))
              #f)
(check-expect (si-game-over?
               (make-fired (make-posn 100 120) (make-tank 100 10) (make-posn 105 115)))
              #t)
(define (si-game-over? s)
  (cond
    [(fired? s) (or
                 (hit? (fired-ufo s) (fired-missile s))
                 (landed? (fired-ufo s))
                 )]
    [(aim? s) (landed? (aim-ufo s))]
    ))

;; Posn Posn -> Boolean
;; determines whether two given positions are close enough(5 pixel proximity)
(check-expect (hit? (make-posn 100 100) (make-posn 94 106)) #f)
(check-expect (hit? (make-posn 100 100) (make-posn 95 105)) #t)
(check-expect (hit? (make-posn 100 100) (make-posn 98 102)) #t)
(define (hit? posn1 posn2)
  (and
   (>= (+ (posn-x posn1) 5) (posn-x posn2) (- (posn-x posn1) 5))
   (>= (+ (posn-y posn1) 5) (posn-y posn2) (- (posn-y posn1) 5))
   ))

;; Posn -> Boolean
;; determines whether ufo has landed(5 pixel proximity)
(check-expect (landed? (make-posn 100 100)) #f)
(check-expect (landed? (make-posn 100 400)) #t)
(check-expect (landed? (make-posn 100 395)) #t)
(define (landed? m)
  (>= (posn-y m) (- BACKGROUND-HEIGHT 5)))

;; Exercise 99

; SIGS -> SIGS
; determine the position that the objects move but
; the UFO moves randomly, so expectations may not be met
; Given:
;   (make-aim (make-posn 5 0)    |  (make-fired (make-posn 5 0)       
;             (make-tank 24 3))  |              (make-tank 24 3)
;                                |              (make-posn 20 350))
; Possible Expect:               |              
;   (make-aim (make-posn 9 5)   |  (make-fired (make-posn 9 5)
;             (make-tank 27 3))  |              (make-tank 27 3)
;                                |              (make-posn 20 340))
(define (si-move SIGS)
  (si-move-proper SIGS (rand-interval UFO-SPEED)))
(check-random (si-move (make-aim (make-posn 5 0)
                                 (make-tank 24 3)))
              (make-aim (make-posn (+ 5 (rand-interval UFO-SPEED)) 5)
                        (make-tank 27 3)))

; SIGS Number -> SIGS
; determine the position that the objects move
; with a set number of steps that UFO can move
; Given:
;   (make-aim (make-posn 5 0)    |  (make-fired (make-posn 5 0)       
;             (make-tank 24 3))  |              (make-tank 24 3)
;   8                            |              (make-posn 20 350))
;                                |   8           
; Expect:                        |              
;   (make-aim (make-posn 13 5)   |  (make-fired (make-posn 13 5)
;             (make-tank 27 3))  |              (make-tank 27 3)
;                                |              (make-posn 20 340))
(define (si-move-proper SIGS steps)
  (cond
    [(aim? SIGS) (si-move-aim-proper SIGS steps)]
    [(fired? SIGS) (si-move-fired-proper SIGS steps)]))
(check-expect (si-move-proper (make-aim (make-posn 5 0)
                                        (make-tank 24 3))
                              8)
              (make-aim (make-posn 13 UFO-SPEED)
                        (make-tank 27 3)))
(check-expect (si-move-proper (make-fired (make-posn 5 0)
                                          (make-tank 24 3)
                                          (make-posn 20 350))
                              8)
              (make-fired (make-posn 13 UFO-SPEED)
                          (make-tank 27 3)
                          (make-posn 20 340)))


; Aim Number -> Aim
; determine the position that the objects move when
; the state of the game is aim. UFO moves the given
; number of steps.
; Given:
;   (make-aim (make-posn 5 0)
;             (make-tank 24 3))
;   8
; Expect:
;   (make-aim (make-posn 13 5))
;             (make-tank 27 3))
(define (si-move-aim-proper aim steps)
  (make-aim
   (si-move-ufo-proper (aim-ufo aim) steps)
   (si-move-tank (aim-tank aim))))
(check-expect (si-move-aim-proper (make-aim (make-posn 5 0)
                                            (make-tank 24 3))
                                  8)
              (make-aim (make-posn 13 UFO-SPEED)
                        (make-tank 27 3)))


; Fired Number -> Fired
; determine the position that objects move when
; the state of the game is fired. UFO moves
; the given number of steps.
; Given:
;   (make-fired (make-posn 5 0)
;               (make-tank 24 3)
;               (make-posn 24 18))
;   8
; Expect:
;   (make-fired (make-posn 13 5)
;               (make-tank 27 3)
;               (make-posn 24 13))
(define (si-move-fired-proper fired steps)
  (make-fired
   (si-move-ufo-proper (fired-ufo fired) steps)
   (si-move-tank (fired-tank fired))
   (si-move-missile (fired-missile fired))))
(check-expect (si-move-fired-proper (make-fired (make-posn 5 0)
                                                (make-tank 24 3)
                                                (make-posn 24 18))
                                    8)
              (make-fired (make-posn 13 UFO-SPEED)
                          (make-tank 27 3)
                          (make-posn 24 8)))

; Tank -> Tank
; moves the tank by delta x units
; changes the direction if tank reached the end of screen
(check-expect (si-move-tank (make-tank 24 3))
              (make-tank 27 3))
(check-expect (si-move-tank (make-tank 13 -3))
              (make-tank 10 -3))
(check-expect (si-move-tank (make-tank 198 5))
              (make-tank 203 -5))
(check-expect (si-move-tank (make-tank 3 -5))
              (make-tank -2 5))
(define (si-move-tank tank)
  (cond
    [(or (>= (+ (tank-vel tank) (tank-loc tank)) BACKGROUND-WIDTH)
         (< (+ (tank-vel tank) (tank-loc tank)) 0))
     (make-tank (+ (tank-vel tank) (tank-loc tank))
                (- 0 (tank-vel tank)))]
    [else (make-tank (+ (tank-vel tank) (tank-loc tank))
                     (tank-vel tank))]
    ))


; UFO Number -> UFO
; moves the ufo down by UFO-SPEED units, and
; a given number of units left/right
(check-expect (si-move-ufo-proper (make-posn 24 0) 8)
              (make-posn 32 UFO-SPEED))
(define (si-move-ufo-proper ufo steps)
  (make-posn (+ (posn-x ufo) steps)
             (+ (posn-y ufo) UFO-SPEED)))

; Missile -> Missile
; moves the missile up by MISSILE-SPEED units
;(check-expect (si-move-missile (make-posn 24 23))
;              (make-posn 24 13))
(define (si-move-missile missile)
  (make-posn (posn-x missile)
             (- (posn-y missile) MISSILE-SPEED)))

; NNN -> Number
; generates a random in the interval [-n,n]
(check-random (rand-interval 13)
              (- (random (* 13 2)) 13))
(define (rand-interval n)
  (- (random (* n 2)) n))