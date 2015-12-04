;;; Exercise 4.35

(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ 1 low) high)))
