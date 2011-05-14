;;; Exercise 2.17
;;; =============

(define (last-pair l)
  (if (empty? (cdr l))
      l
      (last-pair (cdr l))))
