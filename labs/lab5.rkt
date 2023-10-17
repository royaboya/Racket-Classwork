;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;! Problem 1

;; Consider the following definition for a Book:
(define-struct book [title pages])

;; a Book is a (make-book String [List-of String])
;; - title represents the title of the book
;; - pages is a list of the different pages within the book, containing
;; - the text on each page as its own separate string

;; Examples

(define BOOK-FUNDIES1 (make-book "htdp" (list "this is a "
                                         "book about "
                                         "fundies "
                                         "that "
                                         "is very good and interesting!"
                                         "please read the whole thing!")))

(define BOOK-FUNDIES2 (make-book "fundies 1" (list "Fundies 1 Fall 2023"
                                                   "Big bang is so much fun"
                                                   "Pipe Dream is the best game ever"
                                                   "The End!")))
;; Template
;;(define (book-temp b)
;;  (... (book-title b)...)
;;  (...(book-pages b)...))

;; Note: You can assume each book has an even number of pages



;;! Part A

;; Create two functions: contains-page? and contains-page-ci?
;; In contains-page?, check if the given book contains the given page,
;; where the pages must be exactly the same (capitalization and all). In contains-page-ci?, check if the given books contains the given page, where the
;; pages must be the same (capitalization here does NOT matter). Make sure to follow
;; the function design recipe when writing both functions.

;; contains-page: Book page
;; checks if book has page case sensitive
(define (contains-page? b p)
  (cond
    [(empty? b) #false]
    [(string=? (first b) p) #true]
    [else (contains-page? (rest b))]))


(define (contains-page-ci b p)
  (cond
    [(empty? b) '()]
    [(string=? (string-upcase (first b)) (string-upcase (p))) #true]
    [else (contains-page-ci (rest b))]))


;;! Part B

;; Create two more functions: combine-pages and add-footnotes

;; combine-pages should combine every 2 pages in the book into a single page
;; (e.g. when you open a book, you can see 2 pages at once every time you
;; flip a page). You may assume that the book has an even number of pages.

;; add-footnotes takes in a book where each pair of pages is an article
;; followed by its citations (e.g. pages 1, 3, ... are articles, and pages 2,
;; 4, ... are their corresponding citations). Create a function that shortens
;; the book by moving the citations to the bottom of the page, replacing the
;; "Citations:" header with "\n---" ("\n" is a newline). Every article will
;; have an associated citations page.

;; doesn't make a book instance, will fix later

;; takes in list of pages from book and returns list to be put in book


(define (combine-in-b b)
  (cond
    [( or (= 1 (length b)) (empty? b)) '()]
    [else (cons (string-append (list-ref b 0) "\n---" (substring (list-ref b 1) 10)) (combine-in-b (rest (rest b)))) ]
    ;;[else (string-append (first b) (first (rest (rest b))) (combine-in-b (rest (rest b))))]
    
    ))

(define TEST (list "1" "2" "3" "4"))
;;(combine-in-b TEST)

;; T 1 2 - > 1 2

;; does not work because helper function is modified in favor of footnotes function, design is not good
(define (combine-pages b)
  (cond
    [(empty? b) '()]
    [else (make-book (book-title b) (combine-in-b (book-pages b)))]
    ))


(define (add-footnotes b)
  (cond
    [(empty? b) '()]
    [else (combine-pages b)]
    ))



(check-expect (add-footnotes
               (make-book "example"
                          (list "this is my article"
                                "Citations:\nhttps://example.com"
                                )))
               (make-book "example"
                          (list "this is my article\n---\nhttps://example.com"
                                )))

;;! Part C

;; Time to abstract! Given the functions you wrote in the first two parts, create two
;; abstractions that each encompass two of the functions above. After you write your abstractions,
;; rewrite the functions you created in the first two parts below using the abstraction.
;; Note: You do not have to re-write the signature, purpose, and check-expects for the
;; re-written functions, but you should write new tests to make sure the functionality
;; is still correct.




;;! Problem 2


;; You've been recruited by Beff Jezos to help create a new E-Book app
;; using Racket! Your goal will be to use the previous Book data definition
;; along with big-bang to create a program that simulates reading a book.

(require 2htdp/image)
(require 2htdp/universe)

;;! Part A

;; Create the EBook data definition:
;; Each EBook has at least a Book associated with it, and the current page
;; number that the reader is on (note: page 0 would be the book's cover)



;;! Part B

;; Write a function page->image that takes in a single page of a Book,
;; and then draws that page. Be as fancy/minimal as you'd like, but make
;; sure to test your function!
;; (hint: https://docs.racket-lang.org/teachpack/2htdpimage.html)



;;! Part C

;; Now it's time to draw the EBook itself! It'll be easier if you use at least
;; 2 helpers, but feel free to use as many as you think would be helpful!
;; Your function draw-ebook should take in an EBook and produce an image that
;; represents the page the reader is on in the book.

;; two edge cases to consider:
;; - page 0 is the title page
;; - don't forget your base case when you've reached the end of the book!


;;! Part D

;; Finally, let's add some user interaction to our EBook reader! Whenever
;; the user presses the spacebar, flip to the next page in the book! To make
;; things easier, you don't need to implement the ability to go backwards
;; in pages (e.g. once the user flips the page, the previous page(s) are
;; "forgotten". Name this function "key-ebook"



; go-reader : EBook -> EBook
; Read an EBook

#|
(define (go-reader ebook)
  (big-bang ebook
    [to-draw draw-ebook]
    [on-key key-ebook]))
|#
;; (go-reader EBOOK-1)
;; NOTE: MAKE SURE THE FINAL LINE IS COMMENTED OUT WHEN SUBMITTING TO GRADESCOPE!
