;;; Exercise 2.31
;;; =============

(define (square x) (* x x))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (square sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(define tree1 '(1 (2 (3 4) 5) (6 7)))
