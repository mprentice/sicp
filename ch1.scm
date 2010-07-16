;; General helper functions
(define (square n)
  (* n n))

;;; ==============================================
;;; Exercise 1.11
;;; =============
(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1))
         (* 2 (f1 (- n 2)))
         (* 3 (f1 (- n 3))))))
(define (f2 n)
  (define (f2-iter a b c count)
    (cond
     ((= count 0) c)
     ((= count 1) b)
     ((= count 2) a)
     (else (f2-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f2-iter 2 1 0 n))

;;; ==============================================
;;; Exercise 1.16
;;; =============
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (fast-expt' b n)
  ;; invariant: a * b^n remains constant
  (define (fast-expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter a (square b) (/ n 2)))
          (else (fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter 1 b n))
