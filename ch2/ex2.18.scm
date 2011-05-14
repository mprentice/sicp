;;; Exercise 2.18
;;; =============

(define (reverse l)
  (define (reverse-iter l r)
    (if (empty? l)
        r
        (reverse-iter (cdr l) (cons (car l) r))))
  (reverse-iter l '()))
