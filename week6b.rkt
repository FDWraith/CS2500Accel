;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Kevin Zhang and Celine Yan   10/10
;; Week 6 Exercise b

;; Exercise 234

(require 2htdp/universe)
;; ============================ DATA DEFINITION ============================
(define RS1 (list 1 "still alive"))
(define RS2 (list 2 "helloworld"))
;; A RankedSong is (list N String)
;; INTERPRETATION Represents the song and its corresponding ranking

;; ============================ TEST CASES ============================
(define one-list
  '("Asia: Heat of the Moment"
    "U2: One"
    "The White Stripes: Seven Nation Army"))

;; ============================ FUNCTION DEFINITION ============================

;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                        ~~~~~~~~ make-ranking ~~~~~~~
;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; [Listof String] -> ... nested list ...
;; Generates the html page of a table
(check-expect (make-ranking '())
              `(html
                (head
                 (title "Ranked Song Page"))
                (body
                 (table
                  ((border "1"))))))
(check-expect (make-ranking one-list)
              `(html
                (head
                 (title "Ranked Song Page"))
                (body
                 (table
                  ((border "1"))
                  (tr (td "1") (td "Asia: Heat of the Moment"))
                  (tr (td "2") (td "U2: One"))
                  (tr (td "3") (td "The White Stripes: Seven Nation Army"))))))

              
(define (make-ranking los)
  `(html
    (head
     (title "Ranked Song Page"))
    (body
     (table
      ((border "1"))
      ,@(make-ranked-table (ranking los))))))

;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                        ~~~~~~ make-ranked-table ~~~~ helper function for make-ranking
;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; [Listof RankedSong] -> ... nested list ...
;; Recursively parses through ranked songs to generate the tablerows in HTML
(check-expect (make-ranked-table '())
              '())
(check-expect (make-ranked-table (list RS1))
              (list
               `(tr (td "1") (td "still alive"))))
(check-expect (make-ranked-table (list RS1 RS2))
              (list
               `(tr (td "1") (td "still alive"))
               `(tr (td "2") (td "helloworld"))))
(define (make-ranked-table lors) 
  (cond
    [ (empty? lors) '() ]
    [else (cons (make-ranked-row (first lors))
                (make-ranked-table (rest lors)))]))

;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                        ~~~~~~~ make-ranked-row ~~~~~ helper function for make-ranked-table
;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; RankedSong -> ... nested list ...
;; Convert the given RankedSong into HTML Row format
(check-expect (make-ranked-row RS1)
              `(tr (td "1") (td "still alive")))
(check-expect (make-ranked-row RS2)
              `(tr (td "2") (td "helloworld")))
(define (make-ranked-row rs)
  `(tr ,(make-cell (number->string (first rs)) )
       ,(make-cell (second rs)) ))


;; String -> ... nested list ...
;; Converts the given string into HTML Cell format
(check-expect (make-cell "hello")
              `(td "hello"))
(check-expect (make-cell "1")
              `(td "1"))
(define (make-cell s)
  `(td ,s))


;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                        ~~~~~~~~~~ ranking ~~~~~~~~~~ 
;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; [Listof String] -> [Listof RankedSong]
;; Produces a final list with ranked songs
(check-expect (ranking '()) '())
(check-expect (ranking one-list) (list
                                  (list 1 "Asia: Heat of the Moment")
                                  (list 2 "U2: One")
                                  (list 3 "The White Stripes: Seven Nation Army")))
              
(define (ranking los)
  (reverse (add-ranks (reverse los))))

;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;                        ~~~~~~~~~ add-ranks ~~~~~~~~~  helper function for ranking
;;                        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [Listof String] -> [Listof RankedSong]
;; Ranks each song based on descending order in the list
(check-expect (add-ranks '()) '())
(check-expect (add-ranks one-list) (list
                                    (list 3 "Asia: Heat of the Moment")
                                    (list 2 "U2: One")
                                    (list 1 "The White Stripes: Seven Nation Army")))
(check-expect (add-ranks (reverse one-list)) (list
                                              (list 3 "The White Stripes: Seven Nation Army")
                                              (list 2 "U2: One")
                                              (list 1 "Asia: Heat of the Moment")))

(define (add-ranks los)
  (cond
    [(empty? los) '()]
    [else (cons (list (length los) (first los))
                (add-ranks (rest los)))]))



;; Exercise 242

;; A [ Maybe X ] is one of:
;; - #false
;; - X

;; A [Maybe String] is one of:
;; - #false
;; - String
;; INTERPRETATION Represents the possibility of there being a String
;; For example, prompting the person for their favorite color, in which case
;; they may not have one.

;; A [Maybe [List-of String]] is one of:
;; - #false
;; - List-of String
;; INTERPRETATION Represents the possibility of there being a List-of String
;; For example, asking a person for a list of their pets' names, in which case
;; they may have no pets at all.

;; A [List-of [Maybe String]] is one of:
;; - '()
;; - (cons (Maybe String) (List-of [Maybe String]))
;; INTERPRETATION Represents the possibility of there being a String in a List
;; For example, prompting a class for everybody's favorite color, in which case
;; some may not have favorite colors.

;; String [List-of String] -> [Maybe [List-of String]]
;; returns the remainder of los starting with s
;; #false otherwise
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)

(define (occurs s los)
  (cond
    [(empty? los) #false]
    [else (cond
            [(string=? s (first los)) (rest los)]
            [else (occurs s (rest los))])]))
