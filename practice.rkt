;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; ------ requirements ------ ;
(define 2htdp/image)
(define 2htdp/universe)

#|
Exercise 109. Design a world program that recognizes a pattern in a sequence of KeyEvents.
Initially the program shows a 100 by 100 white rectangle. Once your program has encountered the
first desired letter, it displays a yellow rectangle of the same size. After encountering the
final letter, the color of the rectangle turns green. If any “bad” key event occurs, the program
displays a red rectangle.
|#

; ------ constants ------ ;



; ------ definitions ------ ;