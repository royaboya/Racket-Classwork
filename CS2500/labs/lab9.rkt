;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab5-new) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; Consider the following data definition for binary trees:

(define-struct leaf [val])
(define-struct node [left right])

;; A [BinTree-of X] is one of:
;; - (make-leaf X)
;; - (make-node [BinTree-of X] [BinTree-of X])
;; Interpretation - a binary tree of values of type X which is either:
;; - a leaf containing a value
;; - or a node containing two binary trees

;; The goal of this lab will be to write abstractions for working with binary trees, similar to the
;; abstractions we wrote for lists in the previous lab.

;;! Problem 1

;;! Part A
;; Finish the data design for BinTree by writing a template and examples

(define (tree-temp tree)
	(cond 
	     [(leaf? tree) ...]
	     [(node? tree) (... (node-left) ... (node-right))]))

;;! Part B
;; Design a function called tree-height that takes a binary tree and returns the height of the tree.

(define (tree-height tree)
	tree)

;;! Part C
;; Trees are similar to lists, to demostrate this similarity, write a function called tree-flatten
;; that takes a binary tree an converts it into a list. The list should contain the values of the
;; the values of the leaves in left-to-right order.

(define (tree-flatten tree)
	tree)

;;! Part D
;; Design a function tree-reverse that reverses a given tree. Reversing a tree and then flattening it
;; should yield the same list as if the tree was flattened first, then reversed.

(define (tree-reverse tree)
	tree)

;;! Problem 2

;;! Part A
;; Design a function called tree-map that takes a function and a binary tree and applies the function
;; to each value in the tree. The function should return a new tree with the same structure as the
;; original tree, but with the values replaced by the result of applying the function to the original

(define (tree-map tree)
	tree)

;;! Part B
;; Design a function called tree-andmap that takes a predicate and determines whether all values in the
;; tree satisfy the predicate.

(define (tree-andmap tree)
	tree)

;;! Part C
;; Design a function called tree-ormap that takes a predicate and determines whether any values in the
;; tree satisfy the predicate.

(define (tree-ormap tree)
	tree)

;;! Part D
;; Design a function with the following signature:
;; tree-filter: (X -> Boolean) [BinTree-of X] [BinTree-of X] -> [BinTree-of X]
;; The first value passed should be a base value that replaces any values in the second tree that do not
;; satisfy the predicate.

(define (tree-filter tree)
	tree)

;;! Part E
;; Design a function tree-fold that acts like a fold over a tree. It should take in a function to
;; apply to leaves, which is similar to foldr’s base case. It should also take in a function to
;; combine the results of folding subtrees. These two functions should be used to compress a given tree
;; down to a single resulting value. It should have the following signature:
;; tree-fold : (X -> Y) (Y Y-> Y) [BinTree-of X] -> X

(define (tree-fold tree)
	tree)

;;! Problem 3
;; Now we will use the tree-abstractions we defined in problem 2 to reimplement the solutions in
;; problem 1

;;! Part A
;; Design a function even-length-total that takes a binary tree of strings and sums the lengths of all
;; strings with even length. You must use the tree abstractions you defined in problem 2.

(define (even-length-total tree)
	tree)

;;! Part B
;; Reimplement the tree-flatten function using tree abstractions from problem 2.



;;! Part C
;; Reimplement the tree-height function using tree abstractions from problem 2.

;;! Part D
;; Reimplement the tree-reverse function using tree abstractions from problem 2.

;;! Problem 4
;; Now we will reimplment the tree-abstractions we defined in problem 2 using tree-fold.

;;! Part A
;; Reimplement tree-map using tree-fold and no other abstractions.

;;! Part B
;; Reimplement tree-andmap using tree-fold and no other abstractions.

;;! Part C
;; Reimplement tree-ormap using tree-fold and no other abstractions.

;;! Part D
;; Reimplement tree-filter using tree-fold and no other abstractions.





