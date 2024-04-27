#lang lsl


;; Problem 1

;; part p1a
(define-struct counter (val))

(: make-counter-1 (-> (-> Natural Natural)))
(define make-counter-1
  (let ([c (make-counter 0)])
    (lambda ()
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))

(: make-counter-2 (-> (-> Natural Natural)))
(define make-counter-2
  (lambda ()
    (let ([c (make-counter 0)])
      (lambda (inc)
        (begin
          (set-counter-val! c (+ inc (counter-val c)))
          (counter-val c))))))
;; part p1a

;; part p1b
(: counter-distinguish (-> (-> (-> Natural Natural)) Natural))
(define (counter-distinguish counter-function)
  ((counter-function) ((counter-function) 3))
  )

(check-expect (not (equal? (counter-distinguish make-counter-1)
                           (counter-distinguish make-counter-2)))
              #t)




;; part p1b

;; Problem 2
;; part p2a
(: fast-incr (-> (Counter Natural) (Counter Natural) Natural))
(define (fast-incr c1 c2)
  (begin (set-counter-val! c1 (+ (counter-val c1) 1))
         (set-counter-val! c2 (+ (counter-val c2) 1))
         (+ (counter-val c1) (counter-val c2))))
;; part p2a

;; part p2b
(: fast-incr-prop (-> (Counter Natural) (Counter Natural) True))
(define (fast-incr-prop c1 c2)
  (equal? (+ (counter-val c1) (counter-val c2) 2)
          (fast-incr c1 c2)))
;; part p2b

;; part p2c
(: fast-incr-exercise (-> Natural))
(define (fast-incr-exercise)
  (local[
         (define COUNTER (make-counter 10))
         (define COUNTER2 COUNTER)
         (define FAST-RESULT (fast-incr COUNTER COUNTER2))
         ]
    FAST-RESULT

    ))

;; part p2c



;; Problem 3

;; part p3a
(: fast-incr-fixed (Function (arguments [x (Counter Natural)] [y (Counter Natural)])
                                 (result (AllOf Natural
                                                (lambda (_) (not (eq? x y)))
                                                ))))
(define (fast-incr-fixed c1 c2)
  (begin (set-counter-val! c1 (+ (counter-val c1) 1))
         (set-counter-val! c2 (+ (counter-val c2) 1))
         (+ (counter-val c1) (counter-val c2))))
;; part p3a


;; Problem 4

;; part p4a
(define-struct mcons (first rest))
(define-contract MList (OneOf empty? (Mcons Integer MList)))
;; part p4a

;; part p4b
(: mlength (-> MList Natural))
(define (mlength ml)
  (cond [(empty? ml) 0]
        [(mcons? ml) (add1 (mlength (mcons-rest ml)))]))
;; part p4b
