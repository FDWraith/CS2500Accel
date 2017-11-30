;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "lab7-teachpack.rkt")

; A THDL is one of:
; - String
; - Number
; - Boolean
; - 'null
; - Array
; - Hash
 
; An Array is a [Listof THDL]
; This represents a collection of data, essentially a list
 
;(define-struct hash (data))
; A Hash is a (make-hash HashContent)
; This represents a named set of data, similar to a structure
 
; A HashContent is a [Listof KeyValuePair]
; This represents the set of key-value pairs contained in a Hash
 
;(define-struct kv-pair (key value))
; A KeyValuePair is a (make-kv-pair String THDL)
; This represents an entry into a Hash

(define ex1 "Bleh")
(define ex2 5)
(define ex3 #false)
(define ex4 'null)

(define ex5 (list ex1 ex2))

(define kv1 (make-kv-pair "hi" ex2))
(define kv2 (make-kv-pair "nay" ex4))
(define hsh1 (make-hash (list kv1 kv2)))

(define ex6 hsh1)

; THDL -> ???
; template for thdl
(define (template thdl)
  (local (; THDL -> Boolean
          ; Determines if the thdl is a 'null
          (define (null? t)
            (and (symbol? t) (symbol=? t 'null) ))
          )
    (cond
      [(string? thdl) ...]
      [(number? thdl) ...]
      [(boolean? thdl) ...]
      [(null? thdl) ...]
      [(list? thdl) (... (template (first thdl)) ...
                         (array-template (rest thdl)) ...)]
      [(hash? thdl) (... (hashC-template (hash-data thdl)) ...)])))
 

; Array -> ???
; template for array
(define (array-template a)
  (local (
          )
    (cond
      [(empty? a) (...)]
      [else (... (template (first a)) ... (array-template (rest a)))])))


; HashContent -> ???
; template for hashcontent
(define (hashC-template h)
  (local(
         )
    (cond
      [(empty? h) ...]
      [(cons? h) (... (kvp-template (first h)) ...
                      (hashC-template (rest h)) ...)])))

; KeyValuePair -> ???
; template for keyvaluepair
(define (kvp-template kvp)
  (... (kv-pair-key kvp) ...
       (template (kv-pair-value kvp)) ...))



; -----------------------------------------------
#|
Numbers are displayed as plain numbers

Boolean are displayed as true or false

'null is displayed as null

Strings are wrapped in double quotes (\")

Arrays are wrapped in square brackets ([])

Elements in Arrays are separated by a comma and a space

Hashes are wrapped in curly brackets ({})

Hashes’ keys are wrapped in double quotes

Hashes’ key-value pairs are separated by a comma and a space

KeyValuePairs’ keys and values are separated by a colon and a space

Your solution may or may not include trailing commas


(define ex1 "Bleh")
(define ex2 5)
(define ex3 #false)
(define ex4 'null)

(define ex5 (list ex1 ex2))

(define kv1 (make-kv-pair "hi" ex2))
(define kv2 (make-kv-pair "nay" ex4))
(define hsh1 (make-hash (list kv1 kv2)))

(define ex6 hsh1)

|#


; THDL -> String
; Renders thdl as a String
(check-expect (render-data ex1) "\"Bleh\"")
(check-expect (render-data ex2) "5")
(check-expect (render-data ex3) "false")
(check-expect (render-data ex4) "null")
(check-expect (render-data ex5) "[\"Bleh\", 5, ]")
(check-expect (render-data ex6) "{\"hi\": 5, \"nay\": null, }")

(define (render-data thdl)
  (local (; THDL -> Boolean
          ; Determines if the thdl is a 'null
          (define (null? t)
            (and (symbol? t) (symbol=? t 'null) ))

          ;String -> String
          ;Wraps double quotes around given string
          (define (string->double-quotes s)
            (string-append "\"" thdl "\""))
          
          ;Boolean -> String
          ;Renders the string version of the given boolean
          (define (boolean->string b)
            (if b "true" "false"))

          ;String -> String
          ;Renders the string version of an already stringify-ed array
          (define (array->string l-as-string)
            (string-append "[" l-as-string "]"))


          
          )
    (cond
      [(string? thdl) (string->double-quotes thdl)]
      [(number? thdl) (number->string thdl)]
      [(boolean? thdl) (boolean->string thdl)]
      [(null? thdl) (symbol->string thdl)]
      [(list? thdl) (array->string (render-array thdl))]
      [(hash? thdl) (hash->string (render-hash (hash-data thdl)))])))
 
; Array -> String
; Renders the array as a string

(check-expect (render-array '()) "")
(check-expect (render-array ex5) "\"Bleh\", 5, ")

(define (render-array a)
  (local (;String String -> String
          ;Adds commas between entries
          (define (add-commas s1 s2)
            (string-append s1 ", " s2))
          )
  (cond
    [(empty? a) ""]
    [else  (add-commas (render-data (first a)) (render-array (rest a)))])))


; A HashContent is a [Listof KeyValuePair]
; This represents the set of key-value pairs contained in a Hash
 
;(define-struct kv-pair (key value))
; A KeyValuePair is a (make-kv-pair String THDL)
; This represents an entry into a Hash

; HashContent -> ???
; template for hashcontent
(define (render-hash h)
  (local(
         )
    (cond
      [(empty? h) ...]
      [(cons? h) (... (kvp-template (first h)) ...
                      (hashC-template (rest h)) ...)])))

; KeyValuePair -> ???
; template for keyvaluepair
(define (kvp-template kvp)
  (... (kv-pair-key kvp) ...
       (template (kv-pair-value kvp)) ...))  

