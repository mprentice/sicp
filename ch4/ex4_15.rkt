;;; Ex 4.15

(define (halts? p a)
  ;; Return true if p halts when applied to a, false otherwise.
  )

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;;; (try try) -> ???
;;;
;;; Discussion
;;;
;;; If (try try) returns 'halted, that means that (halts? try try) is
;;; false, so (try try) should run forever, but it returned 'halted.
;;; Contradiction.
;;;
;;; If (try try) runs forever, that means that (halts? try try) is true,
;;; so (try try) should halt, but it runs forever. Contradiction.
;;;
;;; Either case violates the specified behavior of `halts?'.
