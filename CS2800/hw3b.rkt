#lang lsl

;; Problem 1


(: exclusive-range? (-> Integer Integer Integer Boolean))
(define (exclusive-range? lo hi n)
 (and (> n lo) (< n hi)))
(check-contract exclusive-range?)


(: exclusive-range?-prop (-> Integer Integer True))
(define (exclusive-range?-prop lo hi)
  (let[(RAND (if (>= (add1 lo) hi) lo (random (add1 lo) hi)))]
  (or (>= (add1 lo) hi) (exclusive-range? lo hi RAND))))

(check-contract exclusive-range?-prop)

(define (outside-range lo hi n) (or (<= n lo) (>= n hi)))
;; <---LO------HI------>

(check-contract exclusive-range?-prop)

;; Problem 2

(define-contract Odd (Immediate (check (lambda (x) (and  (integer? x) (not (even? x)))))
                                (generate (lambda (fuel) (add1 (* 2 (contract-generate Integer)))))))


(: double-plus1 (-> Odd Odd))
(define (double-plus1 n) (+ 1 (* 2 n)))

(check-contract double-plus1)
#|
(check-contract Even
                (Immediate (check (lambda (x) (and (integer? x) (even? x) ))))
                (generate (lambda (fuel) ())))
|#

;; Problem 3

(: divisible-by-3-or-5? (-> Integer Boolean))
(define (divisible-by-3-or-5? n)
  (or (= 0 (remainder n 3)) (= (remainder n 5 ) 0)))
(check-contract divisible-by-3-or-5?)

(define-contract Divis3Or5 (Immediate (check (lambda (x) (divisible-by-3-or-5? x)))
                                (generate (lambda (fuel) (add1 (random fuel))))))

(: divide-3-or-5 (-> Integer Divis3Or5))
(define (divide-3-or-5 n) (if (divisible-by-3-or-5? n) n 0))
(check-contract divide-3-or-5)

