;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname 8b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Kevin Zhang and Celine Yan
; Week 8 Exercise B

; An Xexpr.v3 is one of:
;  – Symbol
;  – String
;  – Number
;  – (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
;  – (cons Symbol [List-of Xexpr.v3])

; An Attribute*.v3 is a [List-of Attribute.v3].
;   
; An Attribute.v3 is a list of two items:
;   (list Symbol String)

; A LoA-or-Xexpr is one of:
;  - Xexpr.v3
;  - Attribute*.v3

; Examples
(define attr1 '(color "green"))
(define attr2 '(width "100"))
(define attr3 '(em "true"))

(define loa1 '())
(define loa2 `(,attr1))
(define loa3 `(,attr2 ,attr3))

(define xexpr0 '(alice))
(define xexpr1 '(bob ((initial "X"))))
(define xexpr2 '(carol (action)))
(define xexpr3 '(the-classic-machine (action ((initial "X")))))
(define xexpr4 'bleh)
(define xexpr5 "hello")
(define xexpr6 29)

; LoA-or-Xexpr -> Boolean
; determines if x is a list of attributes
(check-expect (list-of-attributes? '()) #t)
(check-expect (list-of-attributes? '( (initial "x") (blah "y") )) #t)
(check-expect (list-of-attributes? xexpr1) #f)
(check-expect (list-of-attributes? xexpr4) #f)
(define (list-of-attributes? x)
  (cond
    [(list? x) (cond
                 [(empty? x) #true]
                 [else
                  (local ((define possible-attribute (first x)))
                    (cons? possible-attribute))])]
    [else #false]))

;; Xexpr -> [List-of Xexpr.v3]
;; Returns the content/body of the given xe (if it has)
(check-expect (xexpr-content xexpr0) '())
(check-expect (xexpr-content xexpr1) '())
(check-expect (xexpr-content xexpr2) '((action)))
(check-expect (xexpr-content xexpr3) '((action ((initial "X")) )))
(check-error (xexpr-content xexpr5))
(define (xexpr-content xe)
  (cond
    [(cons? xe) (local ((define xe-without-name (rest xe))
                        )
                  (cond
                    [(empty? xe-without-name) '()]
                    [else
                     (local ((define loa-or-x
                               (first xe-without-name))
                             )
                       (if (list-of-attributes? loa-or-x)
                           (rest xe-without-name)
                           xe-without-name)
                       )]))]
    [else (error "not found")]))

; XExpr.v3 -> [Listof Attribute.v3]
; Returns the attributes of the given xe (if it has)
(check-expect (xexpr-attr xexpr3) '())
(check-expect (xexpr-attr '(meta ((info "bleh")))) '((info "bleh")))
(check-error (xexpr-attr 'bleh))
(define (xexpr-attr xe)
  (cond
    [(cons? xe) (local ((define optional-loa+content (rest xe)))
                  (cond
                    [(empty? optional-loa+content) '()]
                    [else
                     (local ((define loa-or-x
                               (first optional-loa+content)))
                       (if (list-of-attributes? loa-or-x)
                           loa-or-x
                           '()))]))]
    [else (error "not found")]))

;; Xexpr.v3 -> Symbol
;; Returns the name of the given xe (if it has)
(check-expect (xexpr-name xexpr0) `alice)
(check-expect (xexpr-name xexpr1) `bob)
(check-expect (xexpr-name xexpr2) `carol)
(check-expect (xexpr-name xexpr3) `the-classic-machine)
(check-error (xexpr-name xexpr4))
(define (xexpr-name xe)
  (cond
    [(cons? xe) (first xe)]
    [else (error "not found")]))


; [Listof Attribute.v3] Symbol -> [Maybe String]
; Finds the attribute value associated with the given symbol
(check-expect (find-attr loa1 'color) #false)
(check-expect (find-attr loa2 'color) "green")
(check-expect (find-attr loa3 'color) #false)
(check-expect (find-attr loa3 'em) "true")
(define (find-attr loa s)
  (local ((define match-s (assq s loa)) 
          )
    (if (boolean? match-s)
        match-s
        (second match-s))))


; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
 (get '(meta ((content "+1") (itemprop "F"))) "F")
 "+1")
(check-error
 (get '(meta ((content "+1") (itemprop "nah"))) "F"))
(define (get x s)
  (local ((define result (get-xexpr x 'meta 'itemprop 'content s)))
    (if (string? result)
        result
        (error "not found"))))


; Xexpr.v3 Symbol Symbol Symbol String -> [Maybe String]
; Searches through x where the name is m,
; and finds the value of t2 if the value of t1 matches s
;(check-expect (get-xexpr '(meta ((content "+1")) (itemprop "F")) "F") "+1")
(check-expect (get-xexpr '(meta ((content "+1")) (itemprop "nah"))
                         'meta 'itemprop 'content "F") #false)
(check-expect (get-xexpr '(meta ((content "+1")))
                         'meta 'itemprop 'content "beh") #false)
(check-expect (get-xexpr '(bob ((content "a") (itemprop "b")))
                         'meta 'itemprop 'content "b") #false)
(check-expect (get-xexpr xexpr4 'heh 'heh 'heh "heh") #false)
(check-expect (get-xexpr xexpr5 'heh 'heh 'heh "oops") #false)
(check-expect (get-xexpr xexpr6 'oops 'oops 'oops "hehe") #false)
(check-expect (get-xexpr '(machine (meta ((content "a") (itemprop "b"))))
                         'meta 'itemprop 'content "b") "a")
(check-expect (get-xexpr '(machine (action (meta ((content "a") (itemprop "b"))))
                                   (action (meta ((content "b") (itemprop "c")))))
                         'meta 'itemprop 'content "c") "b")
(define (get-xexpr x m t1 t2 s)
  (local (; Xexpr.v3 -> Boolean
          ; Determines if the given xexpr has a list of Attributes
          (define (has-loa? xexpr)
            (and (cons? xexpr)
                 (list-of-attributes? (rest xexpr))))
          ; Xexpr.v3 -> Boolean
          ; Determines if the given xexpr has a name of m
          (define (match? x)
            (symbol=? m (xexpr-name x)))
          ; [Listof Xexpr.v3] -> [Maybe String]
          ; Searches through the list of xexpr with same parameters as above
          (define (search-thru-rest lox)
            (cond
              [(empty? lox) #false]
              [else (local ((define first-val (get-xexpr (first lox) m t1 t2 s)))
                      (if (string? first-val)
                          first-val
                          (search-thru-rest (rest lox))))]))
          )
    (cond
      [(symbol? x) #false]
      [(string? x) #false]
      [(number? x) #false]
      [(has-loa? x) (if (match? x)
                        (extract-content (xexpr-attr x) t1 t2 s)
                        (search-thru-rest (xexpr-content x)))]
      [else (search-thru-rest (xexpr-content x))])))
; ^ I don't know why I am not hitting this test case



; [Listof Attribute.v3] Symbol Symbol String -> [Maybe String]
; returns the value of t2 if the value of t1 matches s in loa
(check-expect (extract-content '((content "hello") (itemprop "woot"))
                               'itemprop 'content "woot") "hello")
(check-expect (extract-content '((content "hello") (itemprop "woot"))
                               'itemprop 'content "hello") #false)
(check-expect (extract-content '((content "hello"))
                               'itemprop 'content "hello") #false)
(check-expect (extract-content '((bleh "hello") (new "woot"))
                               'new 'bleh "woot") "hello")
(define (extract-content loa t1 t2 s)
  (local ((define item-if-found?
            (find-attr loa t1))
          (define matched-tag?
            (and (string? item-if-found?)
                 (string=? item-if-found? s)))
          )
    (if matched-tag?
        (find-attr loa t2)
        #false)))
