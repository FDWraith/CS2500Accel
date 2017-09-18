;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname week3setA) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct week (mon wed thu))
(define-struct lecture (date topic readings))
(define-struct date (month date))
; A Week is (make-week HolidayLecture HolidayLecture HolidayLecture)
; A HolidayLecture is one of: 
; – String
; – (make-lecture Date String String)
; interpretation a String refers to a holiday, 
; a lecture to a date, a topic, and URL for a reading assignment
; A Date is (make-date Number Number)

;; Exercise 2
;; dow (short for day-of-week) is one of:
;; -- "mon"
;; -- "wed"
;; -- "thu"

;; Week dow -> Week
;; changes the given day in the week to a holiday
(check-expect (turn-into-holiday (make-week
                                  (make-lecture (make-date 9 18) "A" "B")
                                  (make-lecture (make-date 9 20) "A" "B")
                                  (make-lecture (make-date 9 21) "A" "B"))
                                 "mon")
              (make-week "holiday"
                         (make-lecture (make-date 9 20) "A" "B")
                         (make-lecture (make-date 9 21) "A" "B")))
(check-expect (turn-into-holiday (make-week
                                  (make-lecture (make-date 9 18) "A" "B")
                                  (make-lecture (make-date 9 20) "A" "B")
                                  (make-lecture (make-date 9 21) "A" "B"))
                                 "wed")
              (make-week (make-lecture (make-date 9 18) "A" "B")
                         "holiday"
                         (make-lecture (make-date 9 21) "A" "B")))
(check-expect (turn-into-holiday (make-week
                                  (make-lecture (make-date 9 18) "A" "B")
                                  (make-lecture (make-date 9 20) "A" "B")
                                  (make-lecture (make-date 9 21) "A" "B"))
                                 "thu")
              (make-week (make-lecture (make-date 9 18) "A" "B")
                         (make-lecture (make-date 9 20) "A" "B")
                         "holiday"))
(define (turn-into-holiday w dow)
  (cond
    [(string=? dow "mon") (make-week "holiday" (week-wed w) (week-thu w))]
    [(string=? dow "wed") (make-week (week-mon w) "holiday" (week-thu w))]
    [(string=? dow "thu") (make-week (week-mon w) (week-wed w) "holiday")]))

;; Exercise 3
;; Week -> Week
;; flips all the number in the date strcutres in given Week

(check-expect (flip (make-week
                     (make-lecture (make-date 9 18) "A" "B")
                     (make-lecture (make-date 9 20) "A" "B")
                     (make-lecture (make-date 9 21) "A" "B")))
              (make-week
               (make-lecture (make-date 18 9) "A" "B")
               (make-lecture (make-date 20 9) "A" "B")
               (make-lecture (make-date 21 9) "A" "B")))

(define (flip w)
  (make-week
   (consume-lecture(week-mon w))
   (consume-lecture(week-wed w))
   (consume-lecture(week-thu w))))

;; HolidayLecture -> HolidayLecture
;; if HolidayLecture is a String, return as is
;; else flips all number in the date structures in given lecture
(check-expect (consume-lecture
               (make-lecture (make-date 9 18) "A" "B"))
              (make-lecture (make-date 18 9) "A" "B"))
(check-expect (consume-lecture "holiday")
              "holiday")
(define (consume-lecture lec)
  (cond
    [(string? lec) lec]
    [else (make-lecture (consume-date (lecture-date lec))
                        (lecture-topic lec)
                        (lecture-readings lec))]))

;; Date -> Date
;; flips all number in the date structures in given Date
(check-expect (consume-date (make-date 9 18))
              (make-date 18 9))

(define (consume-date d)
  (make-date (date-date d)
             (date-month d)))

;; Exercise 4
;; Week String -> Week
;; add String to each readings in Lecture structure at the very beginning

(check-expect (prefix (make-week
                       (make-lecture (make-date 9 18) "A" "B")
                       (make-lecture (make-date 9 20) "A" "DE")
                       (make-lecture (make-date 9 21) "A" "FGH")) "C")
              (make-week
               (make-lecture (make-date 9 18) "A" "CB")
               (make-lecture (make-date 9 20) "A" "CDE")
               (make-lecture (make-date 9 21) "A" "CFGH")))

(define (prefix w r)
  (make-week
   (add-reading (week-mon w) r)
   (add-reading (week-wed w) r)
   (add-reading (week-thu w) r)))

;; Lecture String -> Lecture
;; add String to the readings in Lecture at the very beginning

(check-expect (add-reading (make-lecture (make-date 9 18) "A" "B") "C")
              (make-lecture (make-date 9 18) "A" "CB"))

(define (add-reading lec r)
  (make-lecture (lecture-date lec)
                (lecture-topic lec)
                (string-append r (lecture-readings lec))))