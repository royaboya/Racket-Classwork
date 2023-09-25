#lang racket

(define-struct ball [x y vx vy])
;; Ball is (make-ball Num Num Num Num)
;; Ball is a Moving ball

;; insert ball template because vscode sucks 

(define BALL-0 (make-ball 0 0 0 0))
(define BALL-1 (make-ball 50 50 20 0))
(define BALL-2 (make-ball 50 50 20 10))

(define (move-ball b)
    (make-ball (+ (ball-x b) (ball-vx b)) 
               (+ (ball-y b) (ball-vy b))
               (ball-vx b) (ball-vy b)))
