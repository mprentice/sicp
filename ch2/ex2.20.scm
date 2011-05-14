;;; Exercise 2.20
;;; =============

(define (same-parity a . b)
  (define (same-parity-iter l acc)
    (if (empty? l)
        acc
        (if (or (and (even? a) (even? (car l)))
                (and (odd? a) (odd? (car l))))
            (same-parity-iter (cdr l) (cons (car l) acc))
            (same-parity-iter (cdr l) acc))))
  (reverse (same-parity-iter b (list a))))
