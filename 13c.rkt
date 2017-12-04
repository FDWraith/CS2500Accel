#lang racket

;; Kevin Zhang and Joshua Rosenberg

(module+ test (require rackunit))

(module+ test
  (check-equal? (my-cond [(symbol? "4") "Option1"]
                         [(string? "hi") "It's a string"]
                         [else "Option2"])
                "It's a string")
  
  (check-equal? (my-cond [(symbol? 'Sym) "Option1"]
                         [else "Option2"])
                "Option1")
  (check-equal? (my-cond [(symbol? 4) "Option1"]
                         [else "Option2"])
                "Option2"))
; Syntax: like a cond statement
; Semantics: like a cond statement, but uses nested ifs 
(define-syntax (my-cond stx)
  (syntax-case stx ()
    [(_ [els exp])
     #' exp]
    [(_ [bool-exp exp] [else-or-bool-exp-2 exp-2] ...)
     #' (if bool-exp exp (my-cond [else-or-bool-exp-2 exp-2] ...))]))



(module+ test
  (check-equal? ((fun (x) x) 1)
                ((lambda (x) x) 1))
  (check-equal? (((fun (x y) (* x y)) 2) 3)
                (((lambda (x) (lambda (y) (* x y))) 2) 3)))

; Syntax: multi-argument fun expression
; Semantics: nested single-argument lambda expressions
(define-syntax (fun stx)
  (syntax-case stx ()
    [(_ (param1) expr)
     #'(lambda (param1) expr)]
    [(_ (param1 param2 ...) expr)
     #'(lambda (param1) (fun (param2 ...) expr))]))


