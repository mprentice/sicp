;;; =============
;;; Section 2.3.3
;;; =============

;;; ==============
;;; Unordered sets
;;; ==============

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;; Exercise 2.59
;;; =============
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

;;; Exercise 2.60
;;; =============

(define (adjoin-set x set) (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

;; adjoin-set is now O(1) instead of O(n)
;; union-set is now O(n) where n = len(set1), and could be quite high
;; if set1 contains many duplicates.
;; intersection-set and element-of-set? are unchanged algorithmically,
;; but their runtime has possibly increased for the same reason as
;; union-set.
;; We might use this representation if we want to keep track of how many
;; times an element was added to the set, or if we want adding to the
;; set to be a constant-time operation but don't care about the other
;; operations.

;;; ============
;;; Ordered sets
;;; ============

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;;; Exercise 2.61
;;; =============

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;;; Exercise 2.62
;;; =============

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1)
                                     (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1)
                                     set2)))
        ((< (car set2) (car set1))
         (cons (car set2) (union-set set1
                                     (cdr set2))))))

;;; =========
;;; Tree sets
;;; =========

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;;; Exercise 2.63
;;; =============

(define tree1 '(7 (3 (1 () ())
                     (5 () ()))
                  (9 ()
                     (11 () ()))))

(define tree2 '(3 (1 () ())
                  (7 (5 () ())
                     (9 ()
                        (11 () ())))))

(define tree3 '(5 (3 (1 () ())
                     ())
                  (9 (7 () ())
                     (11 () ()))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; a. Yes, they produce the list '(1 3 5 7 9 11)
;; b. tree->list-1 grows more slowly because append
;;    requires traversing the built list.  If the
;;    left branch is large then this can grow quite
;;    large.

;;; Exercise 2.64
;;; =============

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; a. partial-tree constructs a balanced tree from the list by
;;    recursively building a left subtree from the first half
;;    of the list and a right subtree from the second half, with
;;    the approximately middle element as the root.
;;    '(5 (1 ()
;;           (3 () ()))
;;        (9 (7 () ())
;;           (11 () ())))
;;
;; b. For each of n elements, it splits the list in half and
;;    recursively builds each half.  However, it only has
;;    to traverse the list once to build the tree.  This is
;;    O(n) growth.

;;; Exercise 2.65
;;; =============

;; I'm not sure this is possible in general, because the tree
;; has O(log n) depth.  Here I do it by converting to lists:

(define (union-set set1 set2)
  (define (union-set-list set1 set2)
    (cond ((null? set1) set2)
          ((null? set2) set1)
          ((= (car set1) (car set2))
           (cons (car set1) (union-set-list (cdr set1)
                                            (cdr set2))))
          ((< (car set1) (car set2))
           (cons (car set1) (union-set-list (cdr set1)
                                            set2)))
          ((< (car set2) (car set1))
           (cons (car set2) (union-set-list set1
                                            (cdr set2))))))
  (list->tree (union-set-list (tree->list-1 set1)
                              (tree->list-1 set2))))

(define (intersection-set set1 set2)
  (define (intersection-set-list set1 set2)
    (if (or (null? set1) (null? set2))
        '()    
        (let ((x1 (car set1)) (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1
                       (intersection-set-list (cdr set1)
                                              (cdr set2))))
                ((< x1 x2)
                 (intersection-set-list (cdr set1) set2))
                ((< x2 x1)
                 (intersection-set-list set1 (cdr set2)))))))
  (list->tree (intersection-set-list (tree->list-1 set1)
                                     (tree->list-1 set2))))

;;; Exercise 2.65
;;; =============

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((< (key (entry set-of-records)) given-key)
         (lookup given-key (right-branch set-of-records)))))
