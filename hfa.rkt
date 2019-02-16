;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hfa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (repeat start end f)
  (cond
    [(>= start end) 0]
    [else (begin (display start) (repeat (+ 1 start) end f))])
  )

(repeat 0 10 10)