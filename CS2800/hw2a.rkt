#lang lsl

;; Problem 1: 
;;
;; Translate the following expressions in logic into corresponding function definitions. 
;; Note that the number (and names) of arguments may be different for the different 
;; expressions, as they do not all use the same variables.

;; Template (part p1) to fill in:

;; (P /\ Q) \/  ~(R /\ S)
;; p and q or not(R and S)
(define (p2 P Q R S)
  (or (and P Q) (not (and R S)) )
  )

;; P -> ~Q
(define (p3 P Q) (or (not Q) (not P)))

(define (p4 P Q)
  (boolean=? (not (and P Q)) (or (not P) (not Q))))





;; part p1


;; Problem 2:
;;
;; Prove that the two following equalities (these are De Morgan's Laws) hold for all 
;; possible assignments (i.e., are _valid_) by first defining them (p5 and p6) 
;; and then defining their truth tables using check-expect. 
;; Remember to include all possible combinations of inputs!

;; Template (part p2) to fill in:

;; ~(P /\ Q) = ~P \/ ~Q
(define (p5 P Q)
  (boolean=? (not (and P Q)) (or (not P) (not Q))))
(check-expect (p5 #t #t) #t)
(check-expect (p5 #t #f) #t)
(check-expect (p5 #f #t) #t)
(check-expect (p5 #f #f) #t)

; ...

;; ~(P \/ Q) = ~P /\ ~Q
(define (p6 P Q)
  (boolean=? (not (or P Q)) (and (not P) (not Q))))

(check-expect (p6 #t #t) #t)
(check-expect (p6 #t #f) #t)
(check-expect (p6 #f #t) #t)
(check-expect (p6 #f #f) #t)

; ...


;; part p2

;; Problem 3:
;;
;; For each operator, define a version of it in terms of just `if`.
;; You are welcome to validate your encodings using truth-table tests, but 
;; you are not required. 

;; Template (part p3) to fill in:

;; /\
(define (op_and P Q)
  (if P (if Q #t #f) #f))
(check-expect (op_and #t #t) #t)
(check-expect (op_and #t #f) #f)
(check-expect (op_and #f #t) #f)
(check-expect (op_and #f #f) #f)

;; \/
(define (op_or P Q)
  (if P #t (if Q #t #f)))
(check-expect (op_or #t #t) #t)
(check-expect (op_or #t #f) #t)
(check-expect (op_or #f #t) #t)
(check-expect (op_or #f #f) #f)

;; ->
(define (op_implies P Q)
  (if P (if Q #t #f) (if Q #t #t))
  )
(check-expect (op_implies #t #t) #t)
(check-expect (op_implies #t #f) #f)
(check-expect (op_implies #f #t) #t)
(check-expect (op_implies #f #f) #t)

;; =
(define (op_equal P Q)
  (if P (if Q #t #f) (if Q #f #t)))

(check-expect (op_equal #t #t) #t)
(check-expect (op_equal #t #f) #f)
(check-expect (op_equal #f #t) #f)
(check-expect (op_equal #f #f) #t)


;; ⊕ (exclusive or)
(define (op_xor P Q)
  (if P (if Q #f #t) (if Q #t #f)))

(check-expect (op_xor #t #t) #f)
(check-expect (op_xor #t #f) #t)
(check-expect (op_xor #f #t) #t)
(check-expect (op_xor #f #f) #f)


;; part p3


;; Problem 4:
;;
;; Perform simplifications to remove redundant variables for the three problems below, and include 
;; truth tables that confirm that your simplifications were correct. We are giving you the
;; expressions written both in logical syntax and in the LSL code that we expect
;; you to simplify & test with.

;; Template (part p4) to fill in:

;; (P /\ Q) /\ (R /\ ~Q)

(define (p9 P Q R)
  (and (and P Q) 
       (and R (not Q))))

(define (p9s P Q R) 
  #f)
(check-expect (p9 #t #t #t) (p9s #t #t #t))
(check-expect (p9 #t #f #t) (p9s #t #f #t))
(check-expect (p9 #t #t #f) (p9s #t #t #f))
(check-expect (p9 #f #t #t) (p9s #f #t #t))
(check-expect (p9 #f #f #t) (p9s #f #f #t))
(check-expect (p9 #t #f #f) (p9s #t #f #f))
(check-expect (p9 #f #t #f) (p9s #f #t #f))
(check-expect (p9 #f #f #f) (p9s #f #f #f))



;; (P /\ Q /\ P) \/ (Q /\ R)

;; p and q or q and r -> p and q and r?

;; P and Q OR Q and R
(define (p10 P Q R)
    (or (and P Q P) 
        (and Q R)))

(define (p10s P Q R) 
    (and Q (or P R)))
;; by distributive law
(check-expect (p10 #t #t #t) (p10s #t #t #t))
(check-expect (p10 #t #f #t) (p10s #t #f #t))
(check-expect (p10 #t #t #f) (p10s #t #t #f))
(check-expect (p10 #f #t #t) (p10s #f #t #t))
(check-expect (p10 #f #f #t) (p10s #f #f #t))
(check-expect (p10 #t #f #f) (p10s #t #f #f))
(check-expect (p10 #f #t #f) (p10s #f #t #f))
(check-expect (p10 #f #f #f) (p10s #f #f #f))

;; (P /\ Q /\ R) \/ (~Q /\ S /\ Q)

;; p and q and r OR not q and S and Q
;; p and q and r OR #f -> P Q R and
(define (p11 P Q R S)
    (or (and P Q R)
        (and (not Q) S Q)))
(define (p11s P Q R S)
  (and P Q R))

(check-expect (p11 #t #t #t #t) (p11s #t #t #t #t))

(check-expect (p11 #t #f #t #t) (p11s #t #f #t #t))
(check-expect (p11 #t #t #f #t) (p11s #t #t #f #t))
(check-expect (p11 #f #t #t #t) (p11s #f #t #t #t))
(check-expect (p11 #t #t #t #t) (p11s #t #t #t #f))

(check-expect (p11 #f #f #t #t) (p11s #f #f #t #t))
(check-expect (p11 #t #f #f #t) (p11s #t #f #f #t))
(check-expect (p11 #f #t #f #t) (p11s #f #t #f #t))
(check-expect (p11 #f #t #t #f) (p11s #f #t #t #f))
(check-expect (p11 #t #f #t #f) (p11s #t #f #t #f))

(check-expect (p11 #f #f #f #t) (p11s #f #f #f #t))
(check-expect (p11 #t #f #f #f) (p11s #t #f #f #f))
(check-expect (p11 #f #t #f #f) (p11s #f #t #f #f))
(check-expect (p11 #f #f #t #f) (p11s #f #f #t #t))

(check-expect (p11 #f #f #f #f) (p11s #f #f #f #f))




;; part p4
