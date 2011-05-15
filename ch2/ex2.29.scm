;;; Exercise 2.29
;;; =============

(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (branch-weight branch)
  (if (number? (branch-structure branch))
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (torque branch) (* (branch-length branch)
                           (branch-weight branch)))

(define (balanced? mobile)
  (if (number? mobile)
      #t
      (and (= (torque (left-branch mobile))
              (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

;; balanced mobile
(define mobile1 '((5 8) (2 ((1 10) (1 ((1 5) (1 5)))))))
;; unbalanced mobile
(define mobile2 '((5 8) (2 ((1 10) (2 ((1 5) (1 5)))))))

;; Only need to change selector functions right-branch and
;; branch-structure to use cdr instead of cadr
