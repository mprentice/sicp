;;; Exercise 2.36
;;; =============

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define test-accumulate-n
  (let ((s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
    (equal? '(22 26 30)
            (accumulate-n + 0 s))))
