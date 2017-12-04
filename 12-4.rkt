#lang racket

; #%app   -- application
; #%datum -- primitive pieces of data

(module server racket
  (provide
   (rename-out (nat-s-add1 add1))
   (rename-out (nat-s-sub1 sub1))
   ;; SYNTAX
   ; SEMANTICS
   (rename-out (my-function-application #%app)))

  (define-syntax (my-function-application stx)
    [(_ f a ...)
     #'(#%app f (λ (gO) a) ...)])

  ; [GoTrain Y] is one of:
  ;  -- Y
  ;  -- ['go -> [GoTrain Y]]
  ;  Y is not an ->
  
  ; [X Y] [ [Y -> X] -> {[GoTrain Y] -> X} ]
  (define (run-the-go-closure f pred?)
    (lambda (x)
      (cond
        [(pred? x) (f x)]
        [(pred? x) ((run-the-go-closure f pred?) (x 'go))])))

  
  (define nat-s-add1
    (run-the-go-closure add1 num?))

  (define nat-s-sub1
    (run-the-go-closure sub1 num?))

  (define nat-s-not
    (run-the-go-closure not (λ (x) #true))))

(module client (submod ".." server)
  (require (submod ".." server))

  (define (f x)
    (sub1 x)))


; ----------------------------------------------------------------------------------------------------

(require 2htdp/web-io)

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    [(_ e ...)
     #'(#%plain-module-begin
        (define body (string-join (list 'e ...) ...))
        (define 

(module reader syntax/module-reader
  mark-down ;; name of language
  ;; next three are needed
  #:read
  read
  #:read-syntax
  read-syntax)




   