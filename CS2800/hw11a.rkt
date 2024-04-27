#lang lsl

;; part p0
(define-struct cell (free? value))
(define-contract Cell~ (Cell Boolean Any))

(define-struct memory (pos cells))
(define-contract Memory~ (Memory Natural (List Cell~)))

(define MEMORYSIZE 10)

(define MEMORY
  (make-memory 0 (build-list MEMORYSIZE (lambda (_) (make-cell #t 0)))))
;; part p0


;; Problem 1

;; part p1

#|
(: malloc (-> (Maybe Cell~)))
(define (malloc)
  (local [
          (define loc (memory-cells MEMORY))
          (define initial (find-next-free loc 0))
          
          ]
    (cond
      [(> (memory-pos MEMORY) (length loc)) #f]
      [else (begin
         (set-cell-free?! (list-ref loc (find-next-free loc 0)) #f)
         (set-memory-pos! MEMORY (find-next-free loc 0))
         (list-ref loc initial))]
    
       )))
|#

;; returns index of next free?
(: find-next-free (-> (List Cell~) Natural (Maybe Natural)))
(define (find-next-free loc n)
  (cond
    [(or (empty? loc) (> n (sub1 (length loc)))) (add1 (length loc))]
    [(equal? #t (cell-free? (list-ref loc n))) n]
    [else (find-next-free loc (add1 n))]
    ))




(: malloc (-> (Maybe Cell~)))
(define (malloc)
  (malloc-helper (memory-cells MEMORY))
  )

(define (malloc-helper cell-list)
 (cond
    [(empty? cell-list) #f]
    [(cell-free? (first cell-list))
     (begin (set-cell-free?! (first cell-list) #f)
            (set-memory-pos! MEMORY (add1 (memory-pos MEMORY))) ;; or get next and use list-ref or like reference the next available one
            (first cell-list))]

    [else (malloc-helper (rest cell-list))]
  ))


;; part p1


;; Problem 2

;; part p2
(: free (-> Cell~ False))
(define (free c)
  (begin (set-cell-free?! c #t) #f))
;; part p2

;; Problem 3

;; part p3
(: defrag (-> False))
(define (defrag)
  (local[
         (define FREE (filter (lambda (c) (cell-free? c)) (memory-cells MEMORY)))
         (define NOT-FREE (filter (lambda (c) (not (cell-free? c))) (memory-cells MEMORY)))
         ]

    (begin (set-memory-pos! MEMORY (add1 (length NOT-FREE)))
           (set-memory-cells! MEMORY
                             (append NOT-FREE FREE))
           #f)
    ))
;; part p3


;; Problem 4

;; part p4a
(define (*= c v)
  (set-cell-value! c v))

(define (deref c) (cell-value c))

(: for-loop (-> Cell~ Natural (-> Any) False))
(define (for-loop idx bound body)
  (if (>= (cell-value idx) bound)
      #f
      (begin (body)
             (set-cell-value! idx (add1 (cell-value idx)))
             (for-loop idx bound body))))
;; part p4a


;; part p4b
(: fib (-> Natural (Maybe Natural)))
(define (fib n)
  (begin
    (define fib1 (malloc))
    (define fib2 (malloc))
    (define fibi (malloc))
    (define count (malloc))
    (set-cell-value! fib1 0)
    (set-cell-value! fib2 1)
    (set-cell-value! fibi (deref fib1))
    (set-cell-value! count 0)
    (for-loop count n (λ () (begin
                              (*= fibi (+ (deref fib1)
                                         (deref fib2)))
                              (*= fib1 (deref fib2))
                              (*= fib2 (deref fibi))
                              )))
    (define saved (deref fibi))
    (free fib1)
    (free fib2)
    (free fibi)
    (free count)
    saved
    
    ))
;; part p4b


;; Problem 5
;;
;; Add contracts above, no code down here.