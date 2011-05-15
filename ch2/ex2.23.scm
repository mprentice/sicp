;;; Exercise 2.23
;;; =============

(define (for-each f l)
  (if (empty? l)
      null
      (begin (f (car l))
             (for-each f (cdr l)))))
