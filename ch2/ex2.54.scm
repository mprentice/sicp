;;; Exercise 2.54
;;; =============

(define (equal? a b)
  (if (and (cons? a) (cons? b))
      (and (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
      (eq? a b)))
