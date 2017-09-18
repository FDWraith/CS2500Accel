;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname balldrop) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;(define str "helloworld")
;;(define (insert i c s)
;;  (string-append (substring s 0 i) c (substring s i (string-length s) ) ))
;;(define (rm i s)
;;  (if (>= i (string-length s) )
;;      (substring s 0 i)
;;      (string-append (substring s 0 i) (substring s (+ i 1) (string-length s) ) )))


(require 2htdp/image)

;;Big-Bang
(define (move-ball list-expr)
  (begin
    (define-values (X_m Y_m X_VEL_m Y_VEL_m screen-size_m bounce-x bounce-y)
    (values (car list-expr) (cadr list-expr) (caddr list-expr) (cadddr list-expr) (cadddr (cdr list-expr)) (bounce X_m X_VEL_m BALL_SIZE screen-size_m) (bounce Y_m Y_VEL_m BALL_SIZE screen-size_m) ))
  
    ;;return val
    (list (car bounce-x)
        (car bounce-y)
        (cadr bounce-x)
        (cadr bounce-y)
        screen-size_m)
  ))

(define (bounce position velocity size bound)
  (begin
  (if (and (> (- bound (+ position velocity)) size)
           (> (+ position velocity) size))
      (set! position (+ position velocity))
      (begin
        (set! velocity (* -1 velocity))
        (set! position (+ position velocity))
        ))
  ;;return val
  (list position velocity)
  ))

(define (draw-ball list-expr)
  (begin
  (define X_m (car list-expr))
  (define Y_m (cadr list-expr))
  (define screen-size_m (cadddr (cdr list-expr) ) )
  (place-image (circle BALL_SIZE "solid" "green")
               X_m Y_m
               (empty-scene screen-size_m))))
(define (main screen-size)
  (big-bang (list X Y X_VEL Y_VEL screen-size) 
            [on-tick move-ball]
            [stop-when empty?]
            [to-draw draw-ball]
            ))
  