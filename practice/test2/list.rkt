#lang racket

;; Q1

;; list-ref : Integer [List-of X] -> X
;; produces nth element of list, assuming n less than length of list
(define (list-ref n lst) 
    (cond
        [(= n 0) (first lst)]
        [else (list-ref (- n 1) (rest lst))]))

;;(define TEST (list 1 2 3 4 ))
;;(list-ref 3 TEST)

;; Q2

;; rev : [List-of X] -> [List-of X]
;; reverses elements of list

;; idk how to use a stack-like approach in racket so list-ref is gonna be reused
(define (rev lst) 
    (cond
        [(empty? lst) '()]
        [else (cons (list-ref (- (length lst) 1) lst) (rev (rest lst)))]))

(define TEST2 (list 1 2 3 4 5))
(rev TEST2)