;;; Exercise 2.1
;;; ============

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let* ([np (abs n)]
         [dp (abs d)]
         [g (gcd np dp)]
         [num (/ np g)]
         [den (/ dp g)])
    (if (not (eq? (positive? n)
                  (positive? d)))
        (cons (- num) den)
        (cons num den))))

