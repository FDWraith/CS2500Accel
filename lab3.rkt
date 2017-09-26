;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Non-Empty-List-of-String (NELOS) is one of:
; - (cons String '())
; - (cons String NELOS)

; A List of Strings (LOS) is one-of:
; - '()
; - NELOS

(define-struct animal [name age foods])
; An Animal is a (make-animal String Number NELOS) where
; - name is the animal's name
; - age is the animal's age in years
; - foods is a non-empty list of the animal's favorite foods

; A List of Animals (LOA) is one-of:
; - '()
; - (cons Animal LOA)

(define ANIMAL1-FOOD (cons "cereal" (cons "milk" '())))
(define ANIMAL1 (make-animal "Terry the Tiger" 2 ANIMAL1-FOOD))
 
(define ANIMAL2-FOOD (cons "shrimp" '()))
(define ANIMAL2 (make-animal "Florence Flamingo" 30 ANIMAL2-FOOD))
 
(define ANIMAL3-FOOD
  (cons "spinach"
        (cons "walnuts"
              (cons "balsamic viniagrette" '()))))
(define ANIMAL3 (make-animal "Samantha Stickbug" 8 ANIMAL3-FOOD))


; LOA -> LOS
; finds the favorite foods of all the given animals
(check-expect (shopping-list '()) '())
(check-expect (shopping-list (cons ANIMAL2 '())) (cons "shrimp" '()))
(check-expect (shopping-list (cons ANIMAL1 (cons ANIMAL2 '())))
              (cons "cereal" (cons "milk" (cons "shrimp" '()))))
(define (shopping-list a-loa)
  (cond
    [(empty? a-loa) '()]
    [(cons? a-loa) (merge-lists (animal-foods (first a-loa)) (shopping-list (rest a-loa)))]))


; NELOS LOS -> NELOS
; merging the two given lists into one
(check-expect (merge-lists ANIMAL2-FOOD '())
              ANIMAL2-FOOD)
(check-expect (merge-lists ANIMAL1-FOOD ANIMAL2-FOOD)
              (cons "cereal" (cons "milk" (cons "shrimp" '()))))
(check-expect (merge-lists ANIMAL2-FOOD ANIMAL1-FOOD)
              (cons "shrimp" (cons "cereal" (cons "milk" '()))))
(define (merge-lists l1 l2)
  (cond
    [(empty? l2) l1]
    [(empty? (rest l1)) (cons (first l1) l2)]
    [else (cons (first l1) (merge-lists (rest l1) l2))]))

; List of Posns (LOP)
; (cons Posn '())
; (cons Posn LOP)

; Examples
(define LOP1 (cons (make-posn 3 4) '()))
(define LOP2 (cons (make-posn 5 5) LOP1))
(define LOP3 (cons (make-pons -1 -2) (cons (make-posn -4 3) LOP2)))


; LOP -> LOP
; reflects the given list of posns over both x and y axes
(check-expect (reflect-xy LOP1)
              (cons (make-posn -3 -4) '()))
(check-expect (reflect-xy LOP2)
              (cons (make-posn -5 -5) 
(define (reflect-xy a-lop)
  a-lop)