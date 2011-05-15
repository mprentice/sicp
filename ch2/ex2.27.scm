;;; Exercise 2.27
;;; =============

(define (deep-reverse t)
  (define (deep-reverse-iter t acc)
    (if (empty? t)
        acc
        (let ((item (if (list? (car t))
                        (deep-reverse (car t))
                        (car t))))
          (deep-reverse-iter (cdr t) (cons item acc)))))
  (deep-reverse-iter t '()))
