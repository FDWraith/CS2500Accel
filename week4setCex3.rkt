;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname week4setCex3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Non-Empty List of String (NELOS) is one of:
; (cons String '())
; (cons String NELOS)

; ------ Examples ------ ;
(define l1 (cons "hello" '()))
(define l2 (cons "bye" l1))
(define l3 (cons "tutu" (cons "ru" l2)))

; NELOS String -> NELOS
; Inserts s between each element of L
(check-expect (separate l1 "_")
              (cons "hello" '()))
(check-expect (separate l2 "_")
              (cons "bye" (cons "_" (cons "hello" '()))))
(check-expect (separate l3 "_")
              (cons "tutu"
                    (cons "_"
                          (cons "ru"
                                (cons "_"
                                      (cons "bye"
                                            (cons "_"
                                                  (cons "hello" '()))))))))
(define (separate L s)
  (cond
    [(empty? (rest L)) L]
    [(cons? (rest L))
     (cons (first L)
           (cons s (separate (rest L) s)))]))
