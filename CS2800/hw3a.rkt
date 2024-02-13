#lang lsl

;; Problem 1

(: cyclic-shuffle-prop (-> String True))
(define (cyclic-shuffle-prop str) (string-contains? (string-append str str) str))
(check-contract cyclic-shuffle-prop)



(: cyclic-shuffle (-> String String))


(define (cyclic-shuffle s)
  (local[
         (define RAND (if (< (string-length s) 1) 0 (add1 (random (string-length s)))))

         (define (move str) (string-append
                             (substring str (sub1 (string-length str))
                                        (string-length str))
                             (substring str 0 (sub1 (string-length str)))))


         (define (redo n str)
           (cond
             [(= 0 n) str]
             [else (redo (sub1 n) (move str))]))
         ]
    (if (<= (string-length s) 1) s (redo RAND s))))

(check-contract cyclic-shuffle)


;; Problem 2

(define (gcd-prop a b)
  (= (gcd a b) (gcd-reference a b)))

(define (gcd-reference a b)
  (local [(define (iter x y)
            (if (= y 0)
                x
                (iter y (remainder x y))))]
    (iter a b)))

;(define (gcd-check a b) ())
; (add1 gcd-reference a b)) -> g + 1
; get max(a,b) once as constant G
; recursive check that g+1 to G do not divide a b out
(define (gcd a b)
  (cond
    [(= 0 b) a]
    [(= 0 a) b]
    [(= a b) b]
    [(< a b) (gcd a (remainder b a))]
    [(> a b) (gcd (remainder a b) b)]))

;; Problem 3

(: find-majority-prop (-> (List Natural) Boolean))
(define (find-majority-prop lst)
  (= (find-majority lst) (find-majority-reference lst)))

(define (find-majority-reference lst)
  (local [(define (count-candidate candidate lst)
            (foldl (lambda (el count) (if (= el candidate) (+ count 1) count)) 0 lst))

          (define (is-majority candidate lst)
            (let* ((count (count-candidate candidate lst))
                   (half-length (quotient (length lst) 2)))
              (if (> count half-length) candidate -1)))]

    (let* ((candidate (foldl (lambda (el acc) (if (zero? acc) el acc)) 0 lst))
           (count (count-candidate candidate lst)))
      (if (> count (quotient (length lst) 2)) candidate -1))))


(: find-majority (-> (List Natural) Integer))
(check-contract find-majority)

(define (find-majority lst)
  (local [(define (majority-candidate acc candidate lst)
            (cond
              [(null? lst) candidate]
              [(zero? acc) (majority-candidate 1 (car lst) (cdr lst))]
              [(= candidate (car lst)) (majority-candidate (+ acc 1) candidate (cdr lst))]
              [else (majority-candidate (- acc 1) candidate (cdr lst))]))

          (define (count-candidate candidate lst)
            (foldl (lambda (el count) (if (= el candidate) (+ count 1) count)) 0 lst))

          (define (is-majority candidate lst)
            (let* ((count (count-candidate candidate lst))
                   (half-length (quotient (length lst) 2)))
              (if (> count half-length) candidate -1)))]

    (let* ((candidate (foldl (lambda (el acc) (if (zero? acc) el acc)) 0 lst))
           (count (count-candidate candidate lst)))
      (if (> count (quotient (length lst) 2)) candidate -1))))

(check-expect (find-majority (list 1 2 3)) -1)
(check-expect (find-majority (list)) -1)
(check-expect (find-majority (list 2 2 2 2 2)) 2)