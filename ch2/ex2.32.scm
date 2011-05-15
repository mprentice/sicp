;;; Exercise 2.32
;;; =============

(define set1 '(1 2 3))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (x) (cons (car s) x))
                     rest)))))

;; First we get all the subsets of the rest of the list.
;; Then we get the current element cons'd on to each subset
;; (the map lambda).
;; Finally, we concat the two lists of subsets.
