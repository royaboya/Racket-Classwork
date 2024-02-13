#lang lsl

;; Problem 1:
;;
;; A (height) balanced binary tree has a difference in height of the
;; left and right subtrees of at most 1, where height is the longest path to a leaf. Importantly,
;; this property (or _invariant_) must be mantained when inserting and removing elements
;; from the tree.
;;
;; Your task: define `balanced-prop` as a predicate.
;; You should test your property on several example trees using `check-expect`.
;; You're welcome to use the example trees we provide as some of your tests, but
;; you should also define some of your own.


;; part p1-a
(define-struct leaf [value])
(define-struct node [left right])
(define-contract (Tree X) (OneOf (Leaf X) (Node (Tree X) (Tree X))))
;; part p1-a

;; part p1-b
(define T1 (make-node (make-node (make-leaf 2)
                                 (make-leaf 3))
                      (make-leaf 4)))
;; part p1-b

;; part p1-c
(define T2 (make-node (make-node (make-node (make-leaf 1)
                                            (make-leaf 2))
                                 (make-leaf 3))
                      (make-leaf 4)))
;; part p1-c

;; part p1-d
(define T3 (make-node (make-node (make-leaf 1)
                                 (make-leaf 2))
                      (make-node (make-leaf 3)
                                 (make-leaf 4))))
;; part p1-d
(: balanced-prop (-> (Tree Integer) True))
(define (balanced-prop tree)
  (cond
    [(leaf? tree) #t]
    [(node? tree) (and (recurr (node-left tree)) (recurr (node-right tree)))]
    ))

(define (recurr curr-node)
  (if (leaf? curr-node) #t
  (<= (abs (- (calculate-height (node-left curr-node)) (calculate-height (node-right curr-node)))) 1)))

(: calculate-height (-> (Tree Integer) Integer))
(define (calculate-height t)
  (cond
    [(leaf? t) 0]
    [else (max (add1 (calculate-height (node-left t))) (add1 (calculate-height (node-right t))))]
    ))



#|

compare max(left) (max right)
if leaf, return 0
if node with more node, return 1 + recursives

|#


(check-expect (balanced-prop T1) #t)
;(check-error (balanced-prop T2))
(check-expect (balanced-prop T3) #t)
;; ...