;;; Exercise 2.28
;;; =============

(define x '((1 2) (3 4)))

(define (fringe t)
  (define (fringe-iter t acc)
    (if (empty? t)
        acc
        (if (list? (car t))
            (fringe-iter (cdr t) (fringe-iter (car t) acc))
            (fringe-iter (cdr t) (cons (car t) acc)))))
  (reverse (fringe-iter t '())))
