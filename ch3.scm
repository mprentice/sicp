;; SICP Ch 3 exercises

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
(define make-account
  (lambda (initial-balance password)))
