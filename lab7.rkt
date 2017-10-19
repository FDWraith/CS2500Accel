;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
 
; [List-of String] -> String
; concatenates all the strings in a given list into one string
(check-expect (collapse (list "hello" " " "claudia"))
              "hello claudia")
#|
(define (collapse ls)
  (cond [(empty? ls) ""]
        [else (string-append (first ls) (collapse (rest ls)))]))
|#

; [List-of Image] -> Image
; draws all images in list placed horizontally next to each other
 
(define c (circle 3 'solid 'red))
(define s (square 3 'solid 'blue))
 
;(check-expect (h-cat (list c s)) (beside/align "bottom" c s))

#|
(define (h-cat li)
  (cond [(empty? li) empty-image]
        [else (beside/align "bottom" (first li) (h-cat (rest li)))]))
|#

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; Abstraction of the above two functions
(define (concat f d lx)
  (cond [(empty? lx) d]
        [else (concat f (f (first lx) d) (rest lx))]))



;; [X -> X] ? [List-of X] -> X
(define (combine f x l)
  (cond [(empty? l) x]
        [else (f (first l) (combine f x (rest l)))]))



; [List-of String] -> String
; concatenates all of the strings into one string
(define (collapse ls)
  (local (; X = String , Y = String
          ; String String -> String
          ; adds two strings into one string
          (define (add-strings s1 s2)
            (string-append s1 s2)))
    (concat add-strings "" ls)))