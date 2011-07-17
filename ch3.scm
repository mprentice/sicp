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


