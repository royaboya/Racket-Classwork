#lang racket


;; java all over again but not exactly

;; there are constructors and accessor methods
;; constructors are just [make]-[struct-name]
;; accessors are [struct-name]-property

(define-struct test(f1 f2 f3))
(define yuh (test(1 2 3)))

;; data definitions for structs
;; CLASS EXAMPLE:
;; A book is a (make-book String String Number)
;; A book at a book store
;; title is the title of the book
;; author is who wrote it
;; price is the price charged for the book