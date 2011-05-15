;;; Exercise 2.38
;;; =============

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define a (fold-right / 1 (list 1 2 3)))
(define b (fold-left / 1 (list 1 2 3)))
(define c (fold-right list null (list 1 2 3)))
(define d (fold-left list null (list 1 2 3)))

;; op must be associative to guarantee fold-right op = fold-left op
