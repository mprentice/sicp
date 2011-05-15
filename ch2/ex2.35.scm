;;; Exercise 2.35
;;; =============

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves x)
                         1))
                   t)))

(define test-count-leaves
  (let ((x '((1 2) (3 4))))
    (and (= (count-leaves '()) 0)
         (= (count-leaves x) 4)
         (= (count-leaves (list x x)) 8))))
