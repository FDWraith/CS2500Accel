;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname week4setCrender) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Gobbler -> Image
;; renders the game state as an image
#|
(check-expect (render-world sample-world)
              (place-images
               (list (circle 5 "solid" "brown")
                     (circle 5 "solid" "blue")
                     (square 10 "solid" "green"))
               (list (make-posn 1 1)
                     random-posn
                     (make-posn 10 20))
               BACKG))
(check-expect (render-world sample-world4)
              (place-images
               (list (circle 5 "solid" "brown")
                     (circle 5 "solid" "blue"))
               (list (make-posn 1 1)
                     random-posn4)
               BACKG))
|#
(define (render-world g)
  (render-turkeys (gobble-ai g) (gobble-player g) (render-food (gobble-food g) BACKG)))

;; ListTurkey Turkey Image -> Image
;; adds turkeys to given image       
(check-expect (render-turkeys (cons t-ai '()) t-player2 BACKG)
              (place-images
               (list
                (circle TKY-RADIUS "solid" AI-COLOR)
                (circle TKY-RADIUS "solid" PLAYER-COLOR))
               (list
                (make-posn 1 1)
                (make-posn 95 95))
               BACKG))
(check-expect (render-turkeys
               (cons t-ai
                     (cons
                      (make-turkey TKY-RADIUS AI-COLOR (make-posn 120 120) #false) '()))
               t-player2 BACKG)
              (place-images
               (list
                (circle TKY-RADIUS "solid" AI-COLOR)
                (circle TKY-RADIUS "solid" AI-COLOR)
                (circle TKY-RADIUS "solid" PLAYER-COLOR))
               (list
                (make-posn 1 1)
                (make-posn 120 120)
                (make-posn 95 95))
               BACKG))         
(define (render-turkeys lt tp im)
  (cond
    [(empty? (rest lt))
     (render-turkey (first lt)
                    (render-turkey tp im))]
    [(cons? (rest lt))
     (render-turkeys (rest lt) tp (render-turkey (first lt) im)) ]))

;; Turkey Image -> Image
;; adds a turkey to the given image
(check-expect (render-turkey t-ai BACKG)
              (place-image (circle TKY-RADIUS "solid" AI-COLOR)
                           1 1 BACKG))
(check-expect (render-turkey t-player2 BACKG)
              (place-image (circle TKY-RADIUS "solid" PLAYER-COLOR)
                           95 95 BACKG))
(define (render-turkey t im)
  (place-image
   (circle (turkey-size t) "solid" (turkey-color t))
   (posn-x (turkey-loc t)) (posn-y (turkey-loc t))
   im))
