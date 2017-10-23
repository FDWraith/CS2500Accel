;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname practice) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
;; A SoN is one-of:
;; - Number
;; - String

;; [List-of Number] String -> [List-of SoN]
;; Adds s to the right of each element in lon
(check-expect (put-right '(1) "|") '(1 "|"))
(check-expect (put-right '() "|") '())
(check-expect (put-right '(1 2 3) "|") '(1 "|" 2 "|" 3 "|"))
(define (put-right lon s)
  (local (;; X = Number,  Y = [List-of SoN]
          ;; Number [List-of SoN] -> [List-of SoN]
          (define (loop-body n loson)
            (cons n (cons s loson))))
    (foldr loop-body '() lon)))


;; [List-of Number] -> [List-of Number]
;; Converts the given list of fahrenheit temperatures to celsius
(check-expect (convertFC '(32)) '(0))
(check-expect (convertFC '()) '())
(check-expect (convertFC '(32 212)) '(0 100))
(define (convertFC lon)
  (local (;; X = Number, Y = Number
          ;; Number -> Number
          ;; converts the given fahrenheit temperature to celsius
          (define (f->c n)
            (* 5/9 (- n 32)))
          )
    (map f->c lon)))

;; [List-of Posn] -> [List-of [Pair-of Number Number]]
;; Converts a list of posns to a list of pairs of numbers
(check-expect (translate `( ,(make-posn 12 12) )) '( (12 12) ))
(check-expect (translate '( ) ) '())
(check-expect (translate `( ,(make-posn 12 13)
                            ,(make-posn 4 3)))
              '( (12 13) (4 3)))
(define (translate lop)
  (local (;; X = Posn, Y = [Pair-of Number Number]
          ;; Posn -> [Pair-of Number Number]
          ;; Converts a posn to a pair of numbers
          (define (posn->pair-n p)
            `( ,(posn-x p) ,(posn-y p) ))
          )
    (map posn->pair-n lop)))


#|
Exercise 271. Use ormap to define find-name. The function consumes a name and a list of names.
It determines whether any of the names on the latter are equal to or an extension of the former.
|#

; String [List-of String] -> Boolean
; Determines if any of the strings in the list of strings contain / match the given s
(check-expect (find-name "hello" '("hello" "bob")) #t)
(check-expect (find-name "b" '("hello" "bob")) #t)
(check-expect (find-name "b" '()) #f)
(define (find-name s los)
  (local (; X = String
          ; String -> Boolean
          ; checks if the given string matches / contains s
          (define (name-match? name)
            (string-contains? s name))
          )
    (ormap name-match? los)))



; N -> ... nested list ...
; creates a square of size n with 1's across the top-left to bottom-right diagonal.
(check-expect (diag-square 1) '( (1) ))
(check-expect (diag-square 0) '() )
(check-expect (diag-square 3) '( (1 0 0)
                                 (0 1 0)
                                 (0 0 1) ))
(define (diag-square.v0 n)
  (local (; X = [List-of N]
          ; N -> [List-of N]
          ; constructs a list of zeros of length n, but the r'th index is 1
          (define (diag-row r)
            (local (; X = N
                    ; N -> N
                    ; if k is r, then 1,
                    ; else, 0
                    (define (row-entry k)
                      (if (= k r)
                          1
                          0))
                    )
              (build-list n row-entry)))
          )
    (build-list n diag-row)))

|#

; A Balloon is a (make-posn Number Number)
; represents the balloon's location

; A Needle is a (make-posn Number Number)
; represents the needle's position

; Balloon Needle -> Boolean
; determines if the needle pops the balloon
(define (popped? b n)
  (local ( (define Balloon-X (posn-x b))
           (define Balloon-Y (posn-y b))
           (define Needle-X (posn-x n))
           (define Needle-Y (posn-y n))
          )
    (and (= Balloon-X Needle-X )
         (= Balloon-Y Needle-Y ))))


#|
(define TANK (square 10 "solid" "blue"))
(define MISSILE (circle 5 "solid" "green"))

(define-struct tank [loc vel])
; A Tank is a (make-tank Posn Number)
; A Missile is a Posn

; Tank Missile Image -> Image
; Adds both Missile and Tank images to the given img
(define (render t m img)
  (local ((define TANK-X (posn-x (tank-loc t)))
          (define TANK-Y (posn-y (Tank-loc t)))
          (define MISSILE-X (posn-x m))
          (define MISSILE-Y (posn-y m))
          (define MISSILE-ON-IMG (place-image MISSILE MISSILE-X MISSILE-Y img))
          (define TANK-ON-IMG (place-image TANK TANK-X TANK-Y MISSILE-ON-IMG))
          )
  TANK-ON-IMG))

|#

;; [List-of Number] -> [List-of Number]
;; Converts the given list of fahrenheit temperatures to celsius
(check-expect (convertFC '(32)) '(0))
(check-expect (convertFC '()) '())
(check-expect (convertFC '(32 212)) '(0 100))
  ; (f - 32) * 5/9
(define (convertFC lon)
  (local (;; X = Number, Y = Number
          ;; Number -> Number
          ;; converts a fahrenheit temp to celcius
          (define (convert n)
            (* (- n 32) 5/9))
          )
    (map convert lon)))



;; [List-of Number] String -> [List-of SoN]
;; Adds s to the right of each element in lon
(check-expect (put-right '(1) "|") '(1 "|"))
(check-expect (put-right '() "|") '())
(check-expect (put-right '(1 2 3) "|") '(1 "|" 2 "|" 3 "|"))
(define (put-right lon s)
  (local (;; X = Number, Y = [List-of SoN]
          ;; appends the string to the right
          (define (add-s n losn)
            `(,@losn ,@(list n s)))
          )
    (foldl add-s '() lon)))


