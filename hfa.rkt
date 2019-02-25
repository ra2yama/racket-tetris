;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hfa) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (repeat start end f)
  (cond
    [(>= start end) 0]
    [else (begin (display start) (repeat (+ 1 start) end f))])
  )

(repeat 0 10 10)

;;repeat : Number Number Function -> Number
;;  iterates based on number from (in-range start end)
;;
;;(repeat 0 10 (lambda (n) (* n 1))) -> 0 1 2 3 4 5 6 7 8 9

(define (repeat start end f)
  (cond
    [(>= start (- end 1)) (f start)]
    [else (begin (f start) (repeat (+ 1 start) end f))]))
;; this has begin, though we don't use this function yet
