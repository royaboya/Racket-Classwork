
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw1b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem 1

;; part 1-tests
(check-expect (sum-div-5 (list 1 3 5 7 9 10)) 15)
(check-expect (sum-div-5 (list 1 2 3 4 5)) 5)
(check-expect (sum-div-5 (list -5 2 3 4 5)) 0)
;; part 1-tests

;; sum-div-5 : [ListOf Number] -> Number
;; sums up those numbers divisible by 5
(define (sum-div-5 lon) (foldl +  0 (filter (lambda (n) (= (modulo n 5) 0)) lon)))


;; Problem 2


;; part 2-defn
(define-struct wire (length size leads-to))
(define-struct junction (wires))
(define-struct end ())

;; A Circuit is one of:
;; - (make-wire Natural Natural Circuit)
;; - (make-junction (ListOf Wire))
;; - (make-end)
;; Interpretation: a part of a residential electric circuit, where wires have a length
;; and size in natural numbers (integers >= 0).
;; Examples:
(define W1 (make-wire 10 2 (make-end)))
(define W2 (make-wire 5 1 (make-end)))
(define J1 (make-junction (list W1 W1 W2)))
(define J2 (make-junction (list W2 (make-junction (list W2)) W2)))
;; part 2-defn

;; part 2-tests
(check-expect (circuit-cost W1 5) 100)
(check-expect (circuit-cost J1 5) 225)
(check-expect (circuit-cost J2 10) 150)
;; part 2-tests

;; circuit-cost : Circuit Integer -> Integer
;; calculates the total cost of all wire given a cost per length*size
(define (circuit-cost c volcost)
  (local[
         (define (vol w) (* (wire-length w) (wire-size w)))
         ]

  (cond
    [(wire? c) (+ (* volcost (vol c)) (circuit-cost (wire-leads-to c) volcost))]
    [(and (junction? c) (not (empty? (junction-wires c)))) (+ (circuit-cost (first (junction-wires c)) volcost) (circuit-cost
                                                                         (make-junction (rest (junction-wires c))) volcost))]
    [(end? c) 0]
    [else 0]
    )))


;; Problem 3


;; part 3-defn
(define-struct book (author title pages))

;; A Book is a (make-book String String Natural)
;; Interpretation: a book with an author, title, and number of pages
;; Examples:
(define B1 (make-book "Felleisen" "How to Design Programs" 765))
(define B2 (make-book "MacKenzie" "Mechanizing Proof" 427))
(define B3 (make-book "Pierce" "Types and Programming Languages" 623))
;; part 3-defn

;; part 3-tests
(check-expect (wordy-authors (list B1 B2 B3)) (list "Felleisen" "Pierce"))
(check-expect (wordy-authors (list B2)) (list))
;; part 3-tests


;; wordy-authors : [ListOf Book] -> [ListOf String]
;; returns the names of those authors whose books are above 600 pages
(define (wordy-authors lob) (map (lambda (b) (book-author b)) (filter (lambda (b) (>= (book-pages b) 600)) lob)))