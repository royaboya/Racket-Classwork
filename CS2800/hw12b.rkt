#lang lsl

(define-struct affine-error (v))
(define-struct affine-container (thk))
(define (affine v)
  (let ([a #f])
    (make-affine-container
      (lambda ()
        (if a
            (raise (make-affine-error v))
            (begin (set! a #t)
                   v))))))


(define (affine-get a)
  ((affine-container-thk a)))

(define-contract (Single T)
  (Immediate (check affine-container?)
             (generate (lambda (fuel)
                         (affine (contract-generate T fuel))))))

;; part p0a
(define-struct item (name))
(define-contract Item~ (Item String))
;; part p0a

;; part p0b

(define-contract Inventory (List (Single Item~)))
;; part p0b

;; Problem 1

;; part p1
(: inventory-map (-> (-> Item~ Item~) Inventory Inventory))
(define (inventory-map f i)
    
  (map (λ (x) (affine (f (affine-get x)))) i)
  )

;;(check-contract inventory-map)
;; part p1

;; Problem 2

;; part p2


;; part p2
(: inventory-partition (-> (-> Item~ Boolean) Inventory (Tuple Inventory Inventory)))
(define (inventory-partition p? i)
  (iv-helper i empty empty p?)
  )

(define (iv-helper i itrue ifalse p?)
  (cond
    [(empty? i) (list itrue ifalse)] ;; no more elements base case tuple
    [(cons? i)
     (let ([curr-item (affine-get (first i))])
       (if (p? curr-item)
           (iv-helper (rest i)
                      (append itrue (list (affine curr-item)))
                      ifalse
                      p?)
           (iv-helper (rest i)
                      itrue
                      (append ifalse (list (affine curr-item)))
                      p?)))])

    )





;; part p2


;; Problem 3

;; part p3
(: inventory-get (-> (-> Item~ Boolean) Inventory (Tuple (Maybe (Single Item~)) Inventory)))
(define (inventory-get p? i)
  (local[
         (define p (inventory-partition p? i))
         (define value (if (empty? (first p)) #f (first (first p)))) 
         ]
    (list value (second p))

  ))
;; part p3

;; Problem 4


;; part p4
(: craft-hammer (-> Inventory Inventory))
(define (craft-hammer i)
  (local[
         (define find-wood (inventory-partition (λ (x) (string=? "wood" (item-name x))) i))
         ]
    (if (< (length (first find-wood)) 2)
        (map affine i)
        (local [ (define inv (append (rest (rest (first find-wood))) (second find-wood)))
               (define find-stone (inventory-partition (lambda (x) (string=? (item-name x) "stone")) inv))]
          
          (if (empty? (first find-stone))
              (map affine i)
              (append (rest (first find-stone)) (second find-stone) (list (affine (make-item "hammer"))))))
        
        
        


    )))
;; part p4

