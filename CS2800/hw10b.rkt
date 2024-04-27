#lang lsl

;; Problem 1
;; Define the state for the "a" process

;; part p1
(define-contract AState String)
;; part p1

;; Problem 2
;; Define handlers for the "a" process

;; part p2
(: a-start (-> (List String) (Action AState)))
(define (a-start others)
  (action "" (list (send-packet "b" "hello"))))


(test-suite
 "a-start"
(check-expect(a-start (list "test"))
             (action "" (list (send-packet "b" "hello"))))

(check-expect (a-start empty)
              (action "" (list (send-packet "b" "hello"))))
(check-expect (a-start (list "one" "two"))
                       (action "" (list (send-packet "b" "hello"))))
)


(: a-receive (-> AState (ReceivePacket String) (Action AState)))
(define (a-receive st pkt)
  (if (and (string=? "c" (receive-packet-from pkt))
           (string=? "got it" (receive-packet-msg pkt)))
      (action st (list (send-packet "b" "hello")))
      (action st empty))

  )

(test-suite
 "a-receive"
 (check-expect (a-receive "" (receive-packet "c" "got it"))
               (action "" (list (send-packet "b" "hello"))))
 (check-expect (a-receive "" (receive-packet "b" "got it"))
               (action "" empty))
 (check-expect (a-receive "" (receive-packet "a" "hello"))
               (action "" empty))

 )



;; part p2



;; Problem 3
;; Define state for the "b" process

;; part p3
(define-contract BState String)
;; part p3

;; Problem 4
;; Define handlers for the "b" process

;; part p4
(: b-start (-> (List String) (Action BState)))
(define (b-start others)
  (action "" empty))


(test-suite
 "b-start"
 (check-expect (b-start (list "test"))
               (action "" empty))
 (check-expect (b-start (list "test1" "test 2"))
                        (action "" empty))
 (check-expect (b-start empty)
               (action "" empty))
 )



(: b-receive (-> BState (ReceivePacket String) (Action BState)))
(define (b-receive st pkt)
  (if (and (string=? "a" (receive-packet-from pkt))
           (string=? "hello" (receive-packet-msg pkt)))
      (action st (list (send-packet "c" "hello")))
      (action st empty))
  )

(test-suite
 "b-receive"
 (check-expect (b-receive "" (receive-packet "a" "hello"))
                          (action "" (list (send-packet "c" "hello"))))
 (check-expect (b-receive "" (receive-packet "b" "hello"))
                          (action "" empty))
 (check-expect (b-receive "" (receive-packet "c" "hello"))
               (action "" empty))

 )


;; part p4

;; Problem 5
;; Define state for the "c" process

;; part p5
(define-contract CState String)
;; part p5

;; Problem 6
;; Define handlers for the "c" process

;; part p6
(: c-start (-> (List String) (Action CState)))
(define (c-start others)
  (action "" empty))

(test-suite
 "c-start"
 (check-expect (c-start (list "test"))
               (action "" empty))
 (check-expect (c-start empty)
               (action "" empty))
 (check-expect (c-start (list "test" "test2"))
               (action "" empty))

 )

(: c-receive (-> CState (ReceivePacket String) (Action CState)))
(define (c-receive st pkt)
  (if (and (string=? "b" (receive-packet-from pkt))
           (string=? "hello" (receive-packet-msg pkt))
           (< (string-length st) 4))
           (action (string-append "x" st) (list (send-packet "a" "got it")))
           (action st empty))

  )

(test-suite
 "c-receive"
 (check-expect (c-receive "x" (receive-packet "b" "hello"))
               (action "xx" (list (send-packet "a" "got it"))))
 (check-expect (c-receive "xxxx" (receive-packet "b" "hello"))
               (action "xxxx"  empty))
 (check-expect (c-receive "xxx" (receive-packet "b" "hello"))
               (action "xxxx" (list (send-packet "a" "got it"))))

 )

;; part p6

;; Problem 7
;; Define all the processes using the handlers above:

;; part p7
(define a-process (process (name "a")
                           (on-start a-start)
                           (on-receive a-receive)))
(define b-process (process (name "b")
                           (on-start b-start)
                           (on-receive b-receive)))
(define c-process (process (name "c")
                           (on-start c-start)
                           (on-receive c-receive)))
;; part p7

;; Problem 8
;;
;; Define two functions, main and main-debug, that run the program using start
;; and start-debug respectively. You can use `first` as the scheduler for both.

;; part p8
(define (main)
  (start first (list a-process b-process c-process)))

(define (main-debug)
  (start-debug first (list a-process b-process c-process)))
;; part p8
