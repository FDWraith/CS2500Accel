;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname wk3class1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ------ requirements ------ ;
(require 2htdp/image)
(require 2htdp/universe)

; ------ constants ------ ;
(define COLOR "black")
(define SIZE 66)
(define WIDTH 400)
(define HEIGHT 70)
(deiine BACKGROUND (empty-scene WIDTH HEIGHT))

; ------ definitions ------ ;
(define-struct ks [leftover text])
; A KS is (make-ks Number String)
; INTERPRETATION (make-ks 1 t) represents the situation where
; the program accepts 1 more key strokes and t is the string/text
; entered so far

; Number -> String
; records the first n key strokes of the user
(define (edit n)
  (ks-text
   (big-bang (make-ks n "")
             [to-draw ks-as-image]
             [on-key  add-to-ks]
             [stop-when when-leftover-is-0])))

; KS -> Image
; renders the current KS state as an image on BACKGROUND
(define (ks-as-image)
  empty-image)

; KS KeyEvent -> KS
; adds one keystroke to the current KS's text field
; and decrement the leftover field
(check-expect (add-to-ks (make-ks 1 "") "b") (make-ks 0 "b"))
(check-expect (add-to-ks (make-ks 1 "") "left") (make-ks 1 ""))
(define (add-to-ks a-ks a-ke)
  (cond
    [(> (string-length a-ke) 1) a-ks]
    [else (make-ks (- (ks-leftover a-ks) 1)
                   (string-append (ks-text a-ks) a-ke))]))


; KS -> Boolean
; determines whether the leftover field is (less than or) equal to 0
(define (when-leftover-is-0 a-ks)
  #false)
