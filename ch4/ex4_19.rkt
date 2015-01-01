;;; Ex 4.19

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;;; I agree most with Alyssa's view: better to product an error in the
;;; case of ambiguity or difficulty and let the programmer sort it
;;; out.
;;;
;;; To support Eva's view, we could try all permutations of definition
;;; orders with simulataneous scope and take the first permutation
;;; that doesn't product an error, if any. Calculating all
;;; permutations and trying them is not generally efficient.
