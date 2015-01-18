;;; Ex 4.25

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

;;; Applicative order: infinite recursion (really, stack overflow) on
;;; (* n (factorial (- n 1))) because it's evaluated before trying
;;; `unless'.
;;;
;;; Normal order: works fine because the recursive call isn't
;;; evaluated until it's needed. It will have a stack trace n deep,
;;; however.
