;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname YeetlSeesneecenice) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;join-lists: list list -> list
;;  joins two lists
;;
;;examples: (join-lists (list 1) (list 2)) -> (list 1 2)

(define (join-lists list1 list2)
  (cond
    [(empty? list2) empty]
    [(empty? list1) (cons (first list2) (join-lists list1 list2))]
    [else (cons (first list1) (join-lists (rest list1) list2))]
    )
  )