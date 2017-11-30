;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname BSLinterpreter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; An Expression is one of:
;; -- Number
;; -- String
;; -- Boolean
;; -- (list 'if Expression Expression  Expression)
;; -- (cons 'and (cons Expression (cons Expression [Listof Expression])))
;; -- (cons 'or (cons Expression (cons Expression [Listof Expression])))
;; -- (cons Op-name (cons Expression [Listof Expression]))
;; -- Name 
;; -- (cons Name (cons Expression [Listof Expression])]])

;; A Definition is:
;; -- (list 'define (cons Name (cons Name [Listof Name])) Expression))

;; A Name is a Symbol. 

(define OPS         '(+  >  string-append))
(define OPS-meaning `(,+ ,> ,string-append))
(define OPS-base    `(0  #true ""))
;; An Op-name is a member of OPS.

;; INTERPREPATION When written as a quoted piece of data,
;; an Expressions represents the ISL expression w/o quote.

;; Value is one of:
;; -- Number
;; -- String
;; -- Boolean

;; INTEPRETATION
;; represents the values that drracket produces for the above Expressions 

(define ex1 '5)
(define ex2 '"hello world")
(define ex3 '#true)
(define ex4 '(if #true "go!" "stop!"))
(define ex5 '(and #true #false))
(define ex6 '(or #true #false))
(define ex7 '(+ 1 2 3 4 5))
(define ex8 '(> 5 4 3 2 1))
(define ex9 '(string-append "hello" " " "world"))
(define exA `(if ,ex8 ,ex9 ,ex7))

(define def1 '(define (f x) x))
(define def2 '(define (g x) (+ (f x) (f (+ 2 x)))))
(define def* `(,def1 ,def2))

;; ---------------------------------------------------------------------------------------------------
;; Expression [Listof Definition] -> Value
;; compute what drracket computes for the represented ISL expression

(check-expect (evaluator ex1 '()) 5)
(check-expect (evaluator ex2 '()) "hello world")
(check-expect (evaluator ex3 '()) #true)
(check-expect (evaluator ex4 '()) "go!")
(check-expect (evaluator ex5 '()) #false)
(check-expect (evaluator ex6 '()) #true)
(check-expect (evaluator ex7 '()) 15)
(check-expect (evaluator ex8 '()) #true)
(check-expect (evaluator ex9 '()) "hello world")
(check-expect (evaluator exA '()) "hello world")

;; for coverage (samm refinements)
(check-expect (evaluator '(and #true #true #true) '()) #true)
(check-expect (evaluator '(or #false #false #true) '()) #true)
(check-expect (evaluator '(if #false 0 1) '()) 1)
(check-error (evaluator 'karmen '()))
(check-expect (evaluator '(f 1) def*) 1)
(check-error (evaluator '(f 1 2) def*))

(define (evaluator ex lo-def)
  (cond
    [(number? ex)  ex]    
    [(string? ex)  ex]    
    [(boolean? ex) ex]    
    [(if? ex)
     (if (evaluator (if-condition ex) lo-def)
         (evaluator (if-then ex) lo-def)
         (evaluator (if-else ex) lo-def))]
    [(and? ex)
     (local ((define (eval-lo-def e) (evaluator e lo-def)))
       (and (evaluator (and-first ex) lo-def) 
            (evaluator (and-second ex) lo-def)
            (andmap eval-lo-def (and-rest ex))))]
    [(or? ex)
     (local ((define (eval-lo-def e) (evaluator e lo-def)))
       (or (evaluator (or-first ex) lo-def) 
           (evaluator (or-second ex) lo-def)
           (ormap eval-lo-def (or-rest ex))))]
    [(op-app? ex)
     (local ((define (eval-lo-def e) (evaluator e lo-def)))
       (applyor (op-app-rator ex)
                (evaluator-for-op (op-app-rator ex))
                (evaluator (op-app-rand ex) lo-def)
                (map eval-lo-def (op-app-arguments ex))))]
    [(symbol? ex) (error (string-append (symbol->string ex) " : this variable is not defined"))]
    [else ; (fun-app? ex) 
     (local ((define the-def (evaluator-for-fun (fun-app-name ex) lo-def))
             (define paras   (define-paras the-def))
             (define body    (define-body the-def))
             (define (eval-lo-def e) (evaluator e lo-def))
             (define args
               (cons (evaluator (fun-app-rand ex) lo-def) (map eval-lo-def (fun-app-others ex)))))
       ;; I added a sample error check
       (if (= (length paras) (length args))
           (evaluator (substitute body paras args) lo-def)
           (error (string-append "there are a different number of parameters ("
                                 (number->string (length paras))
                                 ") than arguments ("
                                 (number->string (length args))
                                 ")"))))]))

;; ---------------------------------------------------------------------------------------------------
;; WISH 1

;; Name [Listof Definition] -> Definition 
;; retrieves the RHS and the PARAS of the function definition for fname from lo-def

(check-expect
 (evaluator-for-fun 'kyle
                    '((define (matthias f) (sqr f))
                      (define (kyle kyle) (+ kyle 2))))
 '(define (kyle kyle) (+ kyle 2)))

(check-error
 (evaluator-for-fun 'sqr
                    '((define (matthias f) (sqr f))
                      (define (kyle kyle) (+ kyle 2)))))

(define (evaluator-for-fun name lo-def)
  `(define (,name x) x))

;; ---------------------------------------------------------------------------------------------------
;; Op-meaning is [Value ... -> Value], that is, a function of
;; some number of Values to a Vaue. Very few type systems can
;; express this idea. In Racket, you can use Value *-> Value.

;; Symbol Op-meaning Value [Listof Value] -> Value
;; applies the op meaning to all the values (fst-value others)
(define (applyor op-symbol op fst-value others)
  (apply op (cons fst-value others)))

;; ---------------------------------------------------------------------------------------------------
;; Op-name -> Op-meaning 
;; determine the meaning of this op
(define (evaluator-for-op op)
  (find op OPS OPS-meaning))

;; ---------------------------------------------------------------------------------------------------
;; WISH 2

;; Expression [Listof Name] [Listof Value] -> Expression
;; substitutes all names in lon with their corresponding value in lov all thru ex

(define subex1 '5)
(define subex2 '"hello world")
(define subex3 '#true)
(define subex4 '(if x "go!" "stop!"))
(define subex5 '(and x y))
(define subex6 '(or x #false))
(define subex7 '(+ a b c d e))
(define subex8 '(> a b c d e))
(define subex9 '(string-append x y z))
(define subexA `(if ,subex8 ,subex9 ,subex7))
(define subexB '(celine x y z))

; (check-expect (substitute '(+ x y) '(x y) '(4 5)) '(+ 4 5))
;; (check-expect (substitute '(+ x y) '(a b) '(4 5)) '(+ x y))
(check-expect (substitute subex1 '(x y) '(4 5)) 5)
(check-expect (substitute subex2 '(x y) '(4 5)) "hello world")
(check-expect (substitute subex3 '(x y) '(4 5)) #true)
(check-expect (substitute subex4 '(x) '(#true)) '(if #true "go!" "stop!"))
(check-expect (substitute '(and x y) '(x y) '(#true #false)) '(and #true #false))
(check-expect (substitute subex6 '(x) '(#true)) '(or #true #false))
(check-expect (substitute subex7 '(a b c) '(1 2 3)) '(+ 1 2 3 d e))
(check-expect (substitute subex8 '(b d) '(2 4)) '(> a 2 c 4 e))
(check-expect (substitute subex9 '(x y z) '("hello" " " "world"))
              '(string-append "hello" " " "world"))
(check-expect (substitute subexA
                          '(a b c d e x y z)
                          '(1 2 3 4 5 "woo" " " "hoo"))
              '(if (> 1 2 3 4 5)
                   (string-append "woo" " " "hoo")
                   (+ 1 2 3 4 5)))
(check-expect (substitute subexB
                          '(x y z)
                          '(1 2 3))
              '(celine 1 2 3))
(define (substitute ex lon lov)
  (local (;; X1 = Name X2 = Value Y = Expression
          ;; Name Value Expression -> Expression
          ; susbtitute name by value in ex-eddy
          ;; given:   'x 4 '(+ x y)
          ;; wanted:  '(+ 4 y)
          (define (eddy name value ex)
            (cond
              [(number? ex) ex]    
              [(string? ex) ex]    
              [(boolean? ex) ex]    
              [(if? ex)  `(if ,(eddy name value(if-condition ex))
                              ,(eddy name value(if-then ex)) 
                              ,(eddy name value(if-else ex)))]
              [(and? ex) (local ((define (all-eddy x) (eddy name value x)))
                           `(and ,(eddy name value(and-first ex)) 
                                 ,(eddy name value(and-second ex))
                                 ,@(map all-eddy (and-rest ex))))]
              [(or? ex) (local ((define (all-eddy x) (eddy name value x)))
                          `(or ,(eddy name value(or-first ex))
                               ,(eddy name value(or-second ex))
                               ,@(map all-eddy (or-rest ex))))]
              [(op-app? ex)  (local ((define (all-eddy x) (eddy name value x)))
                               `(,(op-app-rator ex)
                                 ,(eddy name value(op-app-rand ex))
                                 ,@(map all-eddy (op-app-arguments ex))))]
              [(symbol? ex) (if (symbol=? name ex)
                                value
                                ex)]
              [else (local ((define (all-eddy x) (eddy name value x)))
                      `(,(fun-app-name ex)
                        ,(eddy name value (fun-app-rand ex))
                        ,@(map all-eddy (fun-app-others ex))))])))
    (foldr eddy ex lon lov)))

;; ---------------------------------------------------------------------------------------------------
;; Symbol [Listof Symbol] [Listof X] -> X 
;; determine the item of meanings that corresponds to op in names
;; ASSUME names and meanings are of same length
;; ASSUME (= (length names] (length meanings))
(check-expect (find 'b '(a b c) '(1 2 3)) 2)
(check-error  (find 'b '() '()) "can't find b")
(define (find op names meanings)
  (cond
    [(empty? names) (error (string-append "can't find " (symbol->string op)))]
    [else (if (symbol=? (first names) op)
              (first meanings)
              (find op (rest names) (rest meanings)))]))

;; ---------------------------------------------------------------------------------------------------
;; predicates

;; [Listof Symbol] -> [Any -> Boolean]
(define (make-predicate check-for)
  (local (;; ANy -> Boolean 
          (define (predicate? s)
            (and (cons? s) (member? (first s) check-for))))
    predicate?))

(define if?      (make-predicate '(if)))
(define and?     (make-predicate '(and)))
(define or?      (make-predicate '(or)))
(define op-app?  (make-predicate OPS))
(define name?    symbol?)
(define (fun-app? ex) ;; why two rests? 
  (and (cons? ex)
       (name? (first ex))
       (cons? (rest ex))
       (cons? (rest (rest ex)))))

(check-expect (fun-app? '(f 1 2 3)) #true)
(check-expect (fun-app? '(f 1 2 3)) #true)
(check-expect (fun-app? '(f)) #false)
(check-expect (fun-app? '((f 1) 2)) #false)

(define define?  (make-predicate '(define)))

;; ---------------------------------------------------------------------------------------------------
;; accessors

;; [Listof Symbol] N -> [ ... -> ...]
(define (make-accessor tag size)
  (local (;; [ -> ] -> [ -> ]
          (define (f selector)
            (local ((define (g x)
                      ;; ... check with tag and size that it's A ok .. 
                      (selector x)))
              g)))
    f))

(define make-if-sel   (make-accessor '(if) 4))
(define if-condition  (make-if-sel second))
(define if-then       (make-if-sel third))
(define if-else       (make-if-sel fourth))

(define make-and-sel (make-accessor '(and) 3))
(define and-first    (make-and-sel second))
(define and-second   (make-and-sel third))
(define and-rest     (make-and-sel (compose rest rest rest)))

(define make-or-sel (make-accessor '(or) 3))
(define or-first    (make-or-sel second))
(define or-second   (make-or-sel third))
(define or-rest     (make-or-sel (compose rest rest rest)))

(define make-op-app-sel  (make-accessor OPS 2))
(define op-app-rator     (make-op-app-sel first))
(define op-app-rand      (make-op-app-sel second))
(define op-app-arguments (make-op-app-sel (compose rest rest)))

(define make-def-sel (make-accessor '(define) 3))
(define define-fname (make-def-sel (compose first second)))
(define define-paras (make-def-sel (compose rest second)))
(define define-body  (make-def-sel third))

(check-expect (define-fname def1) 'f)
(check-expect (define-paras def1) '(x))
(check-expect (define-body  def1) 'x)

(check-expect (op-app-rator ex7) '+)
(check-expect (op-app-arguments ex7) '(2 3 4 5))

(define fun-app-name first)
(define fun-app-rand second)
(define fun-app-others (compose rest rest))