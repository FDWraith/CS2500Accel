;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cs2c) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ------ requirements ------ ;
(require 2htdp/image)
(require 2htdp/universe)

; ------ constants ------ ;
(define WIDTH 100)
(define HEIGHT 400)
(define BACK (empty-scene WIDTH HEIGHT))
(define SIZE (/ HEIGHT 2) )
(define TANK-HEIGHT (/ SIZE 20) )
(define TANK-WIDTH (/ SIZE 10) )
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "blue"))
(define MISSILE-SIZE (/ TANK-WIDTH 2))
(define MISSILE (triangle (/ SIZE 20) "solid" "green"))
(define MISSILE-SPEED 10)
(define UFO-RADIUS TANK-WIDTH)
(define UFO (circle UFO-RADIUS "solid" "red"))
(define UFO-SPEED 5)

; A UFO is a Posn. 
; INTERPRETATION (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; INTERPRETATION (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; INTERPRETATION (make-posn x y) is the missile's place

; Tank Image -> Image
; renders a Tank onto the given image
; Given:
;   (make-tank 24 3)
;   (empty-scene 200 200)
; Expect:
;   (place-image TANK
;                24 (- 200 TANK-HEIGHT)
;                (empty-scene 200 200))
(define (tank-render tank image)
  (place-image TANK (tank-loc tank) HEIGHT image))

; Missile Image -> Image
; renders the Missile onto the given image
; Given:
;   (make-posn 15 35)
;   (empty-scene 200 200)
; Expect:
;   (place-image MISSILE
;                15 35
;                (empty-scene 200 200))
(define (missile-render missile image)
  (place-image MISSILE (posn-x missile) (posn-y missile) image))

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; INTERPRETATION represents the complete state of a 
; space invader game

; SIGS -> SIGS
; determine the position that the objects move
; Given:ss
;   (make-aim (make-posn 5 0)
;             (make-tank 24 3))
; Expect:
;   (make-aim (make-posn 13 5)
;             (make-tank 24 3))
(define (si-move SIGS)
  (cond
    [(aim? SIGS) (si-move-aim SIGS)]
    [(fired? SIGS) (si-move-fired SIGS)]))

; SIGS -> SIGS
; determine the position that the objects move when
; the state of the game is aim
; Given:
;   (make-aim (make-posn 5 0)
;             (make-tank 24 3))
; Expect:
;   (make-aim (make-posn 13 5))
;             (make-tank 27 3))
(define (si-move-aim SIGS)
  (make-aim
   (si-move-ufo (aim-ufo SIGS))
   (si-move-tank (aim-tank SIGS))))

; SIGS -> SIGS
; determine the position that objects move when
; the state of the game is fired
; Given:
;   (make-fired (make-posn 5 0)
;               (make-tank 24 3)
;               (make-posn 24 18))
; Expect:
;   (make-fired (make-posn 13 5)
;               (make-tank 27 3)
;               (make-posn 24 13))
(define (si-move-fired SIGS)
  (make-fired
   (si-move-ufo (fired-ufo SIGS))
   (si-move-tank (fired-tank SIGS))
   (si-move-missile (fired-missile SIGS))))

; Tank -> Tank
; moves the tank by delta x units
(check-expect (si-move-tank (make-tank 24 3))
              (make-tank 27 3))
(check-expect (si-move-tank (make-tank 13 -3))
              (make-tank 10 -3))
(define (si-move-tank tank)
  (make-tank (+ (tank-vel tank) (tank-loc tank))
             (tank-vel tank)))

; UFO -> UFO
; moves the ufo down by UFO-SPEED units, and
; a random left/right number of units
; Given:
;   (make-posn 13 0)
; Expect:
;   (make-posn 24 5)
(define (si-move-ufo ufo)
  (make-posn (+ (posn-x ufo) (random UFO-SPEED))
             (+ (posn-y ufo) UFO-SPEED)))

; Missile -> Missile
; moves the missile up by MISSILE-SPEED units
(check-expect (si-move-missile (make-posn 24 23))
              (make-posn 24 13))
(define (si-move-missile missile)
  (make-posn (posn-x missile)
             (- (posn-y missile) MISSILE-SPEED)))