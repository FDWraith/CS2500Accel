;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cs2c_full) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ------ requirements ------ ;

(require 2htdp/image)
(require 2htdp/universe)

; ------ structure definitions ----- ;

; A UFO is a Posn.
; INTERPRETATION (make-posn x y) is the UFO's location

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; INTERPRETATION (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; INTERPRETATION (make-posn x y) is the missile's location

(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])
; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; INTEPRETATION represents the complete state of a 
; space invader game

; ------ constants ------ ;

(define BACKGROUND (place-image (star 15 "solid" "white")
                                (random WIDTH) (random (/ HEIGHT 2))
                                (place-image (star 15 "solid" "white")
                                             (random WIDTH) (random (/ HEIGHT 2))
                                             (place-image (star 15 "solid" "white")
                                                          (random WIDTH) (random (/ HEIGHT 2))
                                                          (rectangle WIDTH HEIGHT "solid" "black")))))
(define WIDTH 100)
(define HEIGHT 500)

; SIGS -> Image
; renders the given name state on top of BACKGROUND
