#lang racket

(module+ test (require rackunit))

(define-syntax (at-least-2-args stx)
  (syntax-case stx ()

    [(_ arg1 arg2) #'(+ arg1 arg2)]

    [(_ arg1 arg2 arg3 ...)
     #'(+ arg1 (at-least-2-args arg2 arg3 ...))]

    ))


(at-least-2-args 1 2)
(at-least-2-args 1 2 3)

;; ----------------------------------------
;; AST is one of:              ;; AST is another representation of expressions:
;;  -- (list 'VAR Symbol)      ;;  -- a variable e.g. '(VAR x)
;;  -- (list 'CON Number)      ;;  -- a constant e.g. '(CON 3)
;;  -- (list 'LAM Symbol AST)  ;;  -- a function e.g. '(LAM x (VAR x))
;;  -- (list 'APP AST AST)     ;;  -- an application  '(APP (LAM x (VAR x)) (CON 5))

;; recognizing a syntactic pattern, designing an abstraction
(define-syntax (dispatch stx)
  (syntax-case stx (else)
    [(_ s [else exp])
     #'exp]
    [(_ s [sym exp])
     #'(if (symbol=? 'sym s) exp (error 'dispatch "choices exhausted"))]
    [(_ s [sym exp] [sym-2 exp-2] [sym-more exp-more] ...)
     #'(if (symbol=? 'sym s) exp (dispatch s [sym-2 exp-2] [sym-more exp-more] ...))]
    ))

;; AST -> N
;; measure the size of AST
(define (size.v0 a)
  (cond
    [(symbol=? (first a) 'VAR) 1]
    [(symbol=? (first a) 'CON) 1]
    [(symbol=? (first a) 'LAM) (+ (size (third a)) 1)]
    [(symbol=? (first a) 'APP) (+ (size (second a)) (size (third a)) 1)]))

;; AST -> N
(define (size a)
  (dispatch (first a)
            [VAR 1]
            [CON 1]
            [LAM (+ (size (third a)) 1)]
            [APP (+ (size (second a)) (size (third a)) 1)]
            [else 0]))

(module+ test
  (check-equal? (size '(a b c)) 0))


;; MODULES -- Files plus a way to make some functions (IN)visible
;; SUBMODULES -- Modeuls within a module (module+: distributed source)

(module server racket
  ;; -------------------------------
  (provide
   ;;N -> Number
   (contract-out
    (my-silly-function
     (-> natural-number/c number?))))
  ;; -------------------------------
  (define (my-silly-function n)
    (+ (invisible n 1)))
  ;; N -> Number
  ;; a helper function that is not visible outside the module
  (define (invisible n)
    (random n)))

;; -------------------------------------------------------------------

(module client racket
  ;; -------------------------------
  (require (submod ".." server))
  ;; -------------------------------
  (my-silly-function 10))

;; -------------------------------------------------------------------
(require (submod "." client))

;; MODULES can export SYNTAX

(module serverB racket
  (provide
   ;; SYNTAX    Expression is one of: ... --(ande Expression Expression)
   ;; SEMANTICS represents and in Racket
   (rename-out (ande and)))
  (define-syntax (ande stx)
    (syntax-case ()
      [(_ rand1 rand2)
       #'(begin (displayln `(this is the input to and: ,rand1 rand2))
                (and rand1 rand2))])))

#|
42 --> (list #%datum 42)
you can redefine #%datum using rename-out in a module
you can also redefine function applications 
|#


(module serverC racket
  (provide
   ;; SYNTAX old fun app
   ;; SEMANTIONS delay argument evaluation
   (rename-out (my function-application #%app)))

  (define-syntax (my-function-application stx)
    (syntax-case stx ()
      [(_ f a ...)
       #'(#%app f (λ (go) a) ...)])))

(module clientC racket
  (require (submod ".." server))

  (define (f x)
    







  




  