;;; SICP Chapter 3 exercises

;;; Section 3.1.1

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 100))

(define acc2 (make-account 100))

;; Ex 3.1
(define make-accumulator 
  (lambda (x) 
    (let ((acc x)) 
      (lambda (x) (set! acc (+ acc x)) acc))))

;; Ex 3.2
(define make-monitored
  (lambda (f)
    (let ((numcalls 0))
      (lambda (x)
        (cond ((eq? x 'how-many-calls?) numcalls)
              ((eq? x 'reset-count) (set! numcalls 0))
              (else (begin (set! numcalls (+ 1 numcalls))
                           (f x))))))))

;; Ex 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (bad-password amount)
    "Incorrect password")
  (define (dispatch p m)
    (if (equal? password p)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        bad-password))
  dispatch)

;; Ex 3.4
(define (make-account balance password)
  (let ((attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (bad-password amount)
      "Incorrect password")
    (define (call-the-cops amount)
      "Calling the cops")
    (define (dispatch p m)
      (if (equal? password p)
          (begin (set! attempts 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          (begin (set! attempts (+ attempts 1))
                 (if (> attempts 7)
                     call-the-cops
                     bad-password))))
    dispatch))

;;; Section 3.1.2

;;; Section 3.3.4

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;;; Exercise 3.28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;;; Exercise 3.29

;;; 2 inverter-delay + 1 and-gate-delay
;;; a1 --|>o--+
;;;           |
;;;           +--|--\
;;;              |   |--|>o-- output
;;;           +--|--/
;;;           |
;;; a2 --|>o--+

(define (or-gate-2 a1 a2 output)
  (let ((w1 (make-wire))
        (w2 (make-wire))
        (w3 (make-wire)))
    (inverter a1 w1)
    (inverter a2 w2)
    (and-gate w1 w2 w3)
    (inverter w3 output)
    'ok))

;;; Exercise 3.30

(define (ripple-carry-adder a-list b-list s-list c)
  (if (null? a-list)
      (begin
        (set-signal! c 0)
        'ok)
      (let ((c-in (make-wire)))
        (full-adder (car a-list)
                    (car b-list)
                    c-in
                    (car s-list)
                    c)
        (ripple-carry-adder (cdr a-list)
                            (cdr b-list)
                            (cdr s-list)
                            c-in))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


