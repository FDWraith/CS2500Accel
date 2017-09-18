;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cs2a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;String -> 1String
;finds and returns the first letter of a non-empty string s
;given: "hello world", expect: "h"
;given: "boop", expect: "b"
(define (string-first s)
  (string-ith s 0))

(check-expect (string-first "hello world") "h")
(check-expect (string-first "boop") "b")

;String -> 1String
;finds and returns the last letter of a non-empty string s
;given: "hello world", expect: "d"
;given: "boop", expect: "p"
(define (string-last s)
  (string-ith s (- (string-length s) 1) ))

(check-expect (string-last "hello world") "d")
(check-expect (string-last "boop") "p")

(require 2htdp/image)
;Image -> Number
;computes the area of an image img, represented as the width times the height (in pixels)
;given: (empty-scene 100), expect: 10000
;given: (rectangle 10 30 "solid" "red"), expect: 300
(define (image-area img)
  (* (image-width img)
     (image-height img)))

(check-expect (image-area (empty-scene 100 100)) 10000)
(check-expect (image-area (rectangle 10 30 "solid" "red")) 300)

;String -> String
;removes the first character from a non-empty string s and returns the rest of the string
;given: "hello world", expect: "ello world"
;given: "boop", expect: "oop"
(define (string-rest s)
  (substring s 1 (string-length s)))

(check-expect (string-rest "hello world") "ello world")
(check-expect (string-rest "boop") "oop")

;String -> String
;removes the last character from a non-empty string s and returns the rest of the string
;given: "hello world", expect: "hello worl"
;given: "boop", expect: "boo"
(define (string-remove-last s)
  (substring s 0 (sub1 (string-length s) )))

(check-expect (string-remove-last "hello world") "hello worl")
(check-expect (string-remove-last "boop") "boo")