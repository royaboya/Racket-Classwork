#lang lsl

;; Problem 1

;; part p1a
(define-struct push [num])
(define-struct add [])
(define-struct mul [])
(define-struct sub [])
(define-contract SimpleInstr (OneOf (Push Integer) (Add) (Mul) (Sub)))

(: simple-eval (-> (List Integer) (List SimpleInstr) (List Integer)))
(define (simple-eval stk instrs)
  (local [; stack-binop : [Integer Integer -> Integer] [List-of Integer]
          ;               [List-of SimpleInstr] -> [List-of Integer]
          ; evaluates a binary operator on top two numbers of stack, if present
          (define (stack-binop op stk instrs)
            (if (>= (length stk) 2)
                (simple-eval (cons (op (first stk) (second stk))
                            (rest (rest stk)))
                      instrs)
                (list)))

          ; eval-instr : Instr [List-of Integer] [List-of SimpleInstr] -> [List-of Integer]
          ; evaluates a single instruction, given a stack and rest of instructions
          (define (eval-instr i stk instrs)
            (cond [(add? i) (stack-binop + stk instrs)]
                  [(mul? i) (stack-binop * stk instrs)]
                  [(sub? i) (stack-binop - stk instrs)]
                  [(push? i) (simple-eval (cons (push-num i) stk) instrs)]))]
    (cond [(empty? instrs) stk]
          [(cons? instrs) (eval-instr (first instrs) stk (rest instrs))])))
;; part p1a

;; part p1b
(: simple-stack-verify (-> (List SimpleInstr) (List SimpleInstr) Boolean))
(define (simple-stack-verify p1 p2)
  (equal? (simple-eval '() p1) (simple-eval '() p2)))
(check-contract simple-stack-verify)

;; part p1b


;; Problem 2





;; examples
;; push push add -> push
;; push push push add add -> push push add

;; push push add push push add -> push push

;; lambda that checks if next two are push?


;; push? ignore
;; or
;; general function: if length <= 2 return whatever is in the "stack"
;; else: map or something
;; if push, send to stack
;; if add or whatever, pop 2 out do operation


;; probably use recursion
;; <= 2, return base case
;; else, check 3rd position, if it is, return cons (list (everything before) (new-push)) + (recursive rest rest)

(define TEST (list (make-push 10) (make-push 20) (make-push 20) (make-add) (make-add)))

;; part p2
(: simple-const-fold (-> (List SimpleInstr) (List SimpleInstr)))
(define (simple-const-fold p)
  (local [
          (define REST (if (> (length p) 2) (rest (rest (rest p))) '()))
          ]
  (cond
    [(<= (length p) 2) p]
    [(and (add? (list-ref p 2)) (push? (list-ref p 0)) (push? (list-ref p 1)))
     (cons (make-push (+ (push-num (list-ref p 0))  (push-num (list-ref p 1))))
                                 (simple-const-fold REST))]
    [else (cons (first p) (simple-const-fold (rest p)))]
    )
  ))

(check-contract simple-const-fold)

;; (make-push l) (make-push n) (make-push m) (make-add) (make-add) --> (make-push l) (make-push o)

;; two stacks? once an operation simplification has been done, push temporary stack to final stack

; [l, n, m ADD ADD]
; (make-add) -> pops 2 (m and n) -> (make-push o)

; final stack has [l o]

;; part p2

;; Problem 3

;; part p3
(: simple-const-fold-prop (-> (List SimpleInstr) True))
(define (simple-const-fold-prop p)
  (equal? (simple-eval '() p) (simple-eval '() (simple-const-fold p))))

(check-contract simple-const-fold-prop)


;; part p3

;; Problem 4

;; part p4a
(define-struct var [name])
(define-contract Instr (OneOf (Push Integer) (Add) (Mul) (Sub) (Var String)))

(define-struct bind [name value])
(define-contract Binding (Bind String Integer))


(: eval (-> (List Binding) (List Integer) (List Instr) (List Integer)))
; will return an empty list if it reaches an unbound variable, or a malformed
; program (trying to do an operation without enough values on stack).
(define (eval env stk instrs)
  (local [; stack-binop : [Integer Integer -> Integer] [List-of Integer]
          ;               [List-of Instr] -> [List-of Integer]
          ; evaluates a binary operator on top two numbers of stack, if present
          (define (stack-binop op stk instrs)
            (if (>= (length stk) 2)
                (eval env
                      (cons (op (first stk) (second stk))
                            (rest (rest stk)))
                      instrs)
                (list)))

          ; lookup-var : String [List-of Binding] [List-of Integer]
          ;              [List-of Instr] -> [List-of Integer]
          (define (lookup-var name env stk instrs)
            (cond [(empty? env) (list)]
                  [(cons? env) (if (equal? name (bind-name (first env)))
                                   (eval env
                                         (cons (bind-value (first env))
                                               stk)
                                         instrs)
                                   (lookup-var name (rest env) stk instrs))]))

          ; eval-instr : Instr [List-of Integer] [List-of SimpleInstr] -> [List-of Integer]
          ; evaluates a single instruction, given a stack and rest of instructions
          (define (eval-instr i stk instrs)
            (cond [(add? i) (stack-binop + stk instrs)]
                  [(mul? i) (stack-binop * stk instrs)]
                  [(sub? i) (stack-binop - stk instrs)]
                  [(push? i) (eval env (cons (push-num i) stk) instrs)]
                  [(var? i) (lookup-var (var-name i) env stk instrs)]))]
    (cond [(empty? instrs) stk]
          [(cons? instrs) (eval-instr (first instrs) stk (rest instrs))])))
;; part p4a

;; Your first task is to first define an updated version of `simple-stack-verify`.
;; This time it will take a substitution (set of variable bindings) that it
;; can pass to `eval`.

;; part p4b
(: stack-verify (-> (List Binding) (List Instr) (List Instr) Boolean))
(define (stack-verify env p1 p2)
  (equal? (eval env '() p1) (eval env '() p2))
  )

(check-contract stack-verify)

;; part p4b

;; part p4c
;(: const-fold (-> (List Instr) (List Instr)))
#;(define (const-fold p)
  )



(: const-fold (-> (List Instr) (List Instr)))
(define (const-fold p)
  (local [
          (define REST (if (> (length p) 2) (rest (rest (rest p))) '()))
          ]
  (cond
    [(<= (length p) 2) p]
    [(and (add? (list-ref p 2)) (push? (list-ref p 0)) (push? (list-ref p 1)))
     (cons (make-push (+ (push-num (list-ref p 0))  (push-num (list-ref p 1))))
                                 (simple-const-fold REST))]
    [(and (mul? (list-ref p 2)) (push? (list-ref p 0)) (push? (list-ref p 1)))
     (cons (make-push (* (push-num (list-ref p 0))  (push-num (list-ref p 1))))
                                 (simple-const-fold REST))]

    [(and (sub? (list-ref p 2)) (push? (list-ref p 0)) (push? (list-ref p 1)))
     (cons (make-push (- (push-num (list-ref p 1))  (push-num (list-ref p 0))))
                                 (simple-const-fold REST))]
    
    [else (cons (first p) (const-fold (rest p)))]
    )
  ))

(check-contract const-fold)
;; part p4c




;; part p4d
(: const-fold-prop (-> (List Instr) True))
(define (const-fold-prop l1)
  (stack-verify (list (contract-generate Binding)) l1 (const-fold l1) ))

;(define (stack-verify env p1 p2)



(check-contract const-fold-prop)

;; part p4d


