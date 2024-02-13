#lang lsl

;; Problem 1

;(: duplicate-element (-> (List Natural) Integer))
;; returns a duplicate element of a list if it exists, -1 otherwise

(: duplicate-element (-> (List Natural) Integer))
(define (duplicate-element lon)
  (let ((SORTED (sort lon <)))
  (cond
    [(or (empty? lon) (= 1 (length lon))) -1]
    [(= (first lon) (second lon)) (first lon)]
    [else (duplicate-element (rest lon))]
    )))

(: duplicate-element-prop (-> (List Natural) True))
(define (duplicate-element-prop lon)
  (let ((dupe-result (duplicate-element lon)))
    (if (= -1 dupe-result) #t
        (>= (length (filter (lambda (x) (= x dupe-result)) lon)) 2))))

(check-contract duplicate-element-prop)

;; Problem 2


;; [(2 3 1) (5 5 1) (5 5 1)]
;; [(3 1) (5 5 1) (5 5 1)]
;; [(1) (5 5 1) (5 5 1)] -> 1
;; (first (rest inside list)) -> lambda to check all sublists, andmap true then return, otherwise -1

;; first element of sublist as local?
(: common-element-prop (-> (List (List Natural)) True))
(define (common-element-prop lolon)
  (let ((common-result (common-element lolon)))
  (if (= common-result -1) #t (>= (length (filter (lambda (x) (member? common-result x)) lolon)) 2))))
(check-contract common-element-prop)

(: common-element (-> (List (List Natural)) Integer))
(define (common-element lolon)
  (cond
    [(or (<= (length lolon) 1) (empty? (first lolon))) -1]
    [(andmap (lambda (x) (member? (first (first lolon)) x)) (rest lolon)) (first (first lolon))]
    [else (common-element (cons (rest (first lolon)) (rest lolon)))]
    ))

;(check-contract common-element)



;; Problem 3

;; basically same idea as previous but modified a little
;; 1st element check with rest
;; 2nd element check with rest but its like lambda (x) with a list that is (remove 2nd element)

(define (pair-with-sum-prop lon target)
  (let ((result (pair-with-sum lon target)))
    (if (empty? result) #t (andmap (lambda (x) (member? x lon)) result))
    ))

(: pair-with-sum (-> (List Integer) Integer (List Integer)))
(define (pair-with-sum lon target)
  (let ((result (filter (lambda (n) (= target (+ n (first lon)))) lon)))
    (cond
      [(or (empty? lon) (= 1 (length lon))) '()]
      [(empty? result) (pair-with-sum (rest lon) target)]
      [(not (empty? result)) (list (first lon) (first result))]
      )
    ))

(check-contract pair-with-sum)

;; first -> rest?

;; empty? -> empty list
;; first does work -> return list
;; recursion


#|

[1 2 3 4 5] 5 => '(2, 3)
[1 2] 3 => '(1,2)
length 1 or less: '()





|#