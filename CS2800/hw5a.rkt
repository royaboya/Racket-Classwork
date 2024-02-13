#lang lsl

;; Problem 1:

;; part p1a
(define-contract Bit (OneOf (Constant 0) (Constant 1)))

(define-contract Key (Tuple Bit Bit Bit Bit Bit Bit))

(define-contract Message (Tuple Bit Bit Bit Bit Bit Bit))
;; part p1a

;; part p1b
(: xor (-> Bit Bit Bit))
(define (xor b1 b2)
  (modulo (+ b1 b2) 2))
(check-expect (xor 0 0) 0)
(check-expect (xor 0 1) 1)
(check-expect (xor 1 0) 1)
(check-expect (xor 1 1) 0)

(: xor-list (-> [List Bit] [List Bit] [List Bit]))
(define (xor-list l1 l2)
  (map xor l1 l2))
(check-expect (xor-list (list 1 0 0) (list 1 1 1)) (list 0 1 1))
(check-expect (xor-list (list 0 0 0) (list 0 0 0)) (list 0 0 0))

(: encrypt (-> Message Key Message))
(define encrypt xor-list)

(: decrypt (-> Message Key Message))
(define decrypt xor-list)
;; part p1b

;; part p1c
(: xor-perfect-prop (-> Message Message True))
(define (xor-perfect-prop encr-msg arbitrary-msg)
  (xor-help encr-msg arbitrary-msg))

(: xor-help (-> Message Message True))
(define (xor-help encr-msg arbitrary-msg)
  (local [
          (define generated-key (map (lambda (x) (random 0 2)) (list "" "" "" "" "" "")))
          ]
    (cond
      [(equal? arbitrary-msg (decrypt encr-msg generated-key)) #t]
      [else (xor-help encr-msg arbitrary-msg)]
      )))

;(check-contract xor-perfect-prop)

;; part p1c


;; Problem 2


;; part p2a
(define CORRECT-PASSWORD
  (explode "a7he29hdee"))

(define-contract Password (lambda (s) (and (list? s) (andmap string? s))))

(: password=? (-> Password Password Boolean))
(define (password=? l1 l2) ; l1 is the correct
  (let* (
         (L1 (foldl string-append "" l1))
         (L2 (foldl string-append "" l2))
         )
    (cond
      [(<= (string-length L1) (string-length L2)) (string=? L1 L2)] ;; password is shorter than other
      [else (string=? L1 "a7he29hdea")] ;; if input string is shorter, it's already wrong so just do random
      )
    
  ))



(: check-password (-> Password Boolean))
(define (check-password p)
  (password=? CORRECT-PASSWORD p))
;; part p2a

;; part p2c
(: timing-spec (-> String String True))
(define (timing-spec s1 s2)
  (let ([p1 (explode s1)]
        [p2 (explode s2)])
    (= (ticks (lambda () (check-password p1))) (ticks (lambda () (check-password p2))) )))
;;  [(and (string=? "" s1) (string=? "" s2)) #t]
;; (= (ticks (check-password s1)) (ticks (check-password s2)))

;; if input string is shorter, return a fake one where you do string=? correct <wrong pass of same length>
;; if input string is longer, it's fine to just do string=?


(check-contract timing-spec)
;; part p2c
