#lang lsl

;; Problem 1

;; part p1a
(define STARTBAL 1000)
(define MAX-TRANSFERS 20)

(define-struct bank-state-v1 (balance num-transfers other-banks))
(define-contract BS1 (BankStateV1 Natural Natural (List String)))

(define-struct transfer (bal))
(define-contract Transfer~ (Transfer Natural))
;; part p1a

;; part p1b
(: bank-start-v1 (-> (List String) (Action BS1)))
(define (bank-start-v1 others)
  (begin
    (define BANK (make-bank-state-v1 STARTBAL 0 others))
    (define TRANSFER-AMOUNT (random STARTBAL))
    (set-bank-state-v1-balance!
     BANK (- (bank-state-v1-balance BANK) TRANSFER-AMOUNT)) 
    (action BANK
          (list (send-packet (list-ref others (random (length others)))
                             (make-transfer TRANSFER-AMOUNT))))))

;; part p1b


;; part p1c

(: bank-receive-v1 (-> BS1 (ReceivePacket Transfer~) (Action BS1)))
(define (bank-receive-v1 st pkt)
  (local[
         
         (define TRANSFER-IN (transfer-bal (receive-packet-msg pkt))) ;; to put into this account 
         
         ]
    (begin
      (set-bank-state-v1-balance! st (+ TRANSFER-IN (bank-state-v1-balance st))) ;; add transfer in
      (define TRANSFER-OUT (random (bank-state-v1-balance st))) ;; precalculated transfer out value

      (set-bank-state-v1-num-transfers! st (add1 (bank-state-v1-num-transfers st))) ;; add num transfers
      (if (< (bank-state-v1-num-transfers st) MAX-TRANSFERS)
          ;; reduce from balance then send packet
          (begin
            (set-bank-state-v1-balance! st (- (bank-state-v1-balance st) TRANSFER-OUT)) 
            (action st (list (send-packet (list-ref (bank-state-v1-other-banks st)
                                                    (random (length (bank-state-v1-other-banks st))))
                                          (make-transfer TRANSFER-OUT)))))
          ;; otherwise do nothing
          (action st empty))
      )
    )
  )


#|

(bank-receive-v1 (make-bank-state-v1 100 0 (list "test")) (receive-packet "test"
                                                                            (make-transfer 100)))

|#

;; part p1c

;; part p1d
(define (bank-process-v1 nm)
  (process (name nm)
           (on-start bank-start-v1)
           (on-receive bank-receive-v1)))
;; part p1d

;; part p1e
(define (bank-v1)
  (start first (list (bank-process-v1 "bank1")
                     (bank-process-v1 "bank2")
                     (bank-process-v1 "bank3")
                     (bank-process-v1 "bank4")
                     (bank-process-v1 "bank5")
                     )))
;; part p1e


;; Problem 2

;; part p2a
(define UNTIL-SNAPSHOT 10)

(define-struct bank-state (balance num-transfers other-banks snapshot ignored))
(define-contract BS (BankState Natural Natural (List String) (Maybe Natural) (List String)))

(define-struct marker ())

(define-contract Message (OneOf Transfer~ (Marker)))
;; part p2a


;; part p2b
(: bank-start (-> (List String) (Action BS)))
(define (bank-start others)
  (begin
    (define BANK (make-bank-state STARTBAL 0 others false empty))
    (define TRANSFER-OUT (random STARTBAL))
    (set-bank-state-balance! BANK (- (bank-state-balance BANK) TRANSFER-OUT))
    (action BANK (list (send-packet (list-ref
                                     (bank-state-other-banks BANK)
                                     (random (length (bank-state-other-banks BANK))))
                                    (make-transfer TRANSFER-OUT))))
    ))

;; snapshot receive transfer
;; snapshot dont receive transfer
;; transfer? && snapshot
;; tranfer && not snapshot

;; marker has snap shot
;; marker doesn thave snap shot
;; transfer snap shot
;; transfer no snap shot 
 

(: bank-receive (-> BS (ReceivePacket Message) (Action BS)))
(define (bank-receive st pkt)
  (cond
    ;; marker has not snapshot (boolean)
    [(and (marker? (receive-packet-msg pkt)) (boolean? (bank-state-snapshot st)))
     (begin
       (set-bank-state-snapshot! st (bank-state-balance st))
       (set-bank-state-ignored! st (append (bank-state-ignored st) (list (receive-packet-from pkt))))
       (action st (map (λ (x) (send-packet x (make-marker))) (bank-state-other-banks st)))
       )
     
     ]

    ;; marker and snapshot going on
    [(and (marker? (receive-packet-msg pkt)) (not (boolean? (bank-state-snapshot st))))
     (begin
       (set-bank-state-snapshot! st (bank-state-balance st))
       (set-bank-state-ignored! st (append (bank-state-ignored st) (list (receive-packet-from pkt))))
       (action st empty)
       )]

    ;; transfer and no snapshot
    [(and (transfer? (receive-packet-msg pkt)) (boolean? (bank-state-snapshot st)))
     (begin 
       (if (< (bank-state-num-transfers st) UNTIL-SNAPSHOT)
           (begin
             (set-bank-state-num-transfers! st (add1 (bank-state-num-transfers st)))
             (set-bank-state-balance! st (+ (bank-state-balance st) (transfer-bal (receive-packet-msg pkt))))
             (action st (list (send-packet (list-ref (bank-state-other-banks st)
                                                     (random (length (bank-state-other-banks st))))
                                           (make-transfer (random (bank-state-balance st)))))))
           (begin
             (if (= 1 (random 2))
                 (begin
                   (set-bank-state-num-transfers! st (add1 (bank-state-num-transfers st)))
                   (set-bank-state-balance! st (+ (bank-state-balance st) (transfer-bal (receive-packet-msg pkt))))
                   (action st (list (send-packet (list-ref (bank-state-other-banks st)
                                                           (random (length (bank-state-other-banks st))))
                                                 (make-marker))))
                                                 )
                   (begin (set-bank-state-balance! st (+ (bank-state-balance st) (transfer-bal (receive-packet-msg pkt))))
                          (set-bank-state-num-transfers! st (add1 (bank-state-num-transfers st)))
                          (action st (list (send-packet (list-ref (bank-state-other-banks st)
                                                                  (random (length (bank-state-other-banks st))))
                                                        (make-transfer (random (bank-state-balance st))))))))
                                               )))]
    ;; transfer and snapshot
    [(and (transfer? (receive-packet-msg pkt)) (not (boolean? (bank-state-snapshot st))))
     (begin
       (set-bank-state-num-transfers! st (add1 (bank-state-num-transfers st)))
       (set-bank-state-balance! st (+ (bank-state-balance st) (transfer-bal (receive-packet-msg pkt))))
       (set-bank-state-snapshot! st (bank-state-balance st))
       (action st empty)
       #;(action st (list (send-packet (list-ref (bank-state-other-banks st)
                                     (random (length (bank-state-other-banks st))))
                                     (make-marker))))
                                
       )]

    ))
#|
(bank-receive (make-bank-state 100 0 (list "test") false empty) (receive-packet "test"
                                                                            (make-transfer 100)))
|#


(define (bank-process nm)
  (process (name nm)
           (on-start bank-start)
           (on-receive bank-receive)))

(define (bank)
  (start first (list (bank-process "bank1")
                     (bank-process "bank2")
                     (bank-process "bank3")
                     (bank-process "bank4")
                     (bank-process "bank5"))))



;; part p2b

(define (snapshot-correct? lob)
  (= (foldl + 0 (map (λ (x) (bank-state-snapshot (second x))) lob))
     (* STARTBAL (length lob))
     )
  )

