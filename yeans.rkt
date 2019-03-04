;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname yeans) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(list (make-posn 0 0) (make-posn 1 0)
                                  (make-posn 0 1) (make-posn 1 1))

(define (lowest-posn-list my-list val)
  (cond
    [(empty? my-list) val]
    [(> (posn-y (first my-list)) (posn-y val)) (lowest-posn-list (rest my-list) (first my-list))]
    [else (lowest-posn-list (rest my-list) val)]))

(lowest-posn-list (list (make-posn 0 0) (make-posn 1 0)
                                  (make-posn 0 1) (make-posn 1 1)) (make-posn -inf.0 -inf.0))

