;;; Exercise 4.36

;; Simply using an-integer-starting-from will get stuck in the
;; innermost loop trying all numbers to infinity (or numeric
;; overflow). To fix this we need to constrain all inner loops so only
;; the outer loop is unbounded.
n
(define (an-integer-between low high)
  (require (< low high))
  (amb low (an-integer-between (+ 1 low) high)))

(define (a-pythagorean-triple-starting-from low)
  (let ((k (an-integer-starting-from low)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
	(require (= (+ (* i i)
		       (* j j))
		    (* k k)))
	(list i j k)))))
