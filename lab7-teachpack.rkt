#lang racket
(require json)
(require rackunit)

(provide (rename-out [make-data-hash make-hash]
                     [data-hash-data hash-data]
                     [data-hash? hash?])
         (struct-out kv-pair)
         string->data
         get-pokemon-data
         get-data
         mock-get-data
         MOCK-DATA)


;; DATA DEFINITIONS


;; A Data is one of:
;; - String
;; - Number
;; - Boolean
;; - 'null
;; - Array
;; - Hash

;; An Array is a [Listof Data]
;; This represents a collection of data, essentially a list

(define-struct data-hash (data) #:transparent)
;; A Hash is a (make-data-hash [Listof KeyValuePair])
;; This represents a named set of data, similar to a structure

(define-struct kv-pair (key value) #:transparent)
;; A KeyValuePair is a (make-kv-pair String Data)
;; This represents an entry into a Hash

;; Data is a BSL-compatible reskin of JSON
;; We are using this reskinned version to eliminate the confusion of the new 
;; terminology and let it relate back to concepts that students are already
;; familiar with.


;; PARSING JSON STRINGS

;; string->data : RawJsonString -> Data
;; Parses the given JSON string into Data representation
(define (string->data string)
  (jsexpr->data (string->jsexpr string)))


;; CONVERTING JSON TO DATA

;; jsexpr->data : JSExpression -> Data
;; Converts the given jsexpr into the Data format
(define (jsexpr->data jsexpr)
  (cond [(boolean? jsexpr) jsexpr]
        [(string? jsexpr) jsexpr]
        [(number? jsexpr) jsexpr]
        [(and (symbol? jsexpr) (symbol=? jsexpr 'null)) 'null]
        [(list? jsexpr) (jsarray->data jsexpr)]
        [(hash? jsexpr) (jshash->data jsexpr)]))


;; jsarray->data : [Listof JSExpression] -> Array
;; Converts the given js array to the Data representation
(define (jsarray->data jsarray)
  (map (λ (element) (jsexpr->data element)) jsarray))


;; jshash->data : [Hash Symbol JSExpression] -> Hash
;; Converts the given JS hash into the Data Hash representation
(define (jshash->data jshash)
  (data-hash (jshash-content->data (hash->list jshash))))


;; jshash-content->data :
;;  [Listof (cons Symbol JSExpression)] -> [Listof KeyValuePair]
;; Converts the given hash data into the Data representation
(define (jshash-content->data jshash-content)
  (map (λ (content)
         (kv-pair (symbol->string (car content))
                  (jsexpr->data (cdr content))))
       jshash-content))



;; RETRIEVING DATA FROM API

(require net/url)

;; get-url : String -> String
;; Returns the raw string results of a GET request on the given URL
;; Borrowed from a previous years' teachpack
(define (get-url str)
  (string-trim (bytes->string/utf-8
                (port->bytes (get-pure-port (string->url str))))))


(define POKEAPI-BASE-URL "https://pokeapi.co")
(define POKEMON-RESOURCE "/api/v1/pokemon/")

;; get-pokemon-data : PositiveInteger -> Data
;; Queries the pokemon API and returns data on the pokemon with the given
;; pokedex number. If the API doesn't gives an empty response, will cause error
(define (get-pokemon-data pdex-number)
  (get-data (string-append POKEMON-RESOURCE (number->string pdex-number) "/")))

;; get-data : String -> Data
;; Gets data found at the given URI in the pokemon API. If the API gives an
;; empty response, will cause an error.
(define (get-data resource-uri)
  (local ([define url (string-append POKEAPI-BASE-URL resource-uri)]
          [define response (get-url url)])
    (if (string=? response "")
        (error (string-append
                "Something went wrong trying to get info from " url "."))
        (string->data response))))

;; The mock-data response
;; Comes from a call to (get-data "/api/v1/game/1/")
(define MOCK-DATA
  (make-data-hash
   (list
    (make-kv-pair "modified" "2013-11-03T19:31:10.975393")
    (make-kv-pair "release_year" 1996)
    (make-kv-pair "id" 1)
    (make-kv-pair "resource_uri" "/api/v1/game/1/")
    (make-kv-pair "name" "Red(jpn)")
    (make-kv-pair "created" "2013-11-03T19:31:10.975452")
    (make-kv-pair "generation" 1))))

;; mock-get-data : String -> Data
;; Returns a predefined Data (mocks the get-data function for testing)
(define (mock-get-data resource_uri)
  MOCK-DATA)



;; TESTS

(check-equal? (jsexpr->data #t) #t)
(check-equal? (jsexpr->data #f) #f)
(check-equal? (jsexpr->data "") "")
(check-equal? (jsexpr->data "foo") "foo")
(check-equal? (jsexpr->data "bar") "bar")
(check-equal? (jsexpr->data 17) 17)
(check-equal? (jsexpr->data -37.8) -37.8 0.001)
(check-equal? (jsexpr->data 'null) 'null)
(check-equal? (jsexpr->data '()) '())
(check-equal? (jsexpr->data (list 1 2)) (list 1 2))
(check-equal? (jsexpr->data #hasheq()) (data-hash '()))
(check-equal? (jsexpr->data #hasheq([foo . null]))
              (data-hash (list (kv-pair "foo" 'null))))

(check-equal? (jsarray->data '()) '())
(check-equal? (jsarray->data (list "1" 2 #f)) (list "1" 2 #f))
(check-equal? (jsarray->data (list 2 (list "foo" #t)))
              (list 2 (list "foo" #t)))
(check-equal? (jsarray->data (list #hasheq([foo . 10]
                                           [bar . 12])
                                   5))
              (list (data-hash (list (kv-pair "bar" 12) (kv-pair "foo" 10)))
                    5))

(check-equal? (jshash->data #hasheq()) (data-hash '()))
(check-equal? (jshash->data #hasheq([foo . 17]
                                    [bar . #f]))
              (data-hash (list (kv-pair "bar" #f) (kv-pair "foo" 17))))
;; Hash in a hash
(check-equal? (jshash->data #hasheq([foo . #hasheq([bar . "bye"])])) 
              (data-hash (list
                          (kv-pair "foo"
                                        (data-hash (list (kv-pair
                                                          "bar" "bye")))))))
;; List in a hash
(check-equal? (jshash->data #hasheq([stuff . ("1" 4 #t)])) 
              (data-hash (list (kv-pair "stuff"
                                             (list "1" 4 #t)))))


(check-equal? (jshash-content->data '()) '())
(check-equal? (jshash-content->data '((foo . 42)))
              (list (kv-pair "foo" 42)))
(check-equal? (jshash-content->data '((foo . #hasheq((bar . "baz")))))
              (list (kv-pair
                      "foo" (data-hash (list (kv-pair "bar" "baz"))))))
(check-equal? (jshash-content->data '((foo . "hi")
                                      (nice . ("a" "b"))))
              (list (kv-pair "foo" "hi")
                         (kv-pair "nice" (list "a" "b"))))

(check-equal? (string->data "1") 1)
(check-equal? (string->data "\"foo\"") "foo")
(check-equal? (string->data "true") #t)
(check-equal? (string->data "null") 'null)
(check-equal? (string->data "[\"foo\", 2, false]") (list "foo" 2 #f))
(check-equal? (string->data "{\"foo\": 12, \"bar\": null}")
              (data-hash (list (kv-pair "bar" 'null) (kv-pair "foo" 12))))

;; Uncomment tests below to check API functions (may significantly increase time
;; it takes to run test suite)
;(check-true (data-hash? (get-data "/api/v1/move/1/")))
;(check-exn exn:fail? (λ () (get-data "/api/v1/move/0/")))

;(check-true (data-hash? (get-pokemon-data 650)))
;(check-exn exn:fail? (λ () (get-pokemon-data 0)))

(check-equal? (mock-get-data "this doesnt actually matter")
              MOCK-DATA)

"all tests run"

