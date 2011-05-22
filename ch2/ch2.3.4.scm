;;; =============
;;; Section 2.3.4
;;; =============

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;; Exercise 2.67
;;; =============
;;; '(A D A B B C A)

;;; Exercise 2.68
;;; =============

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (memq? element set)
  (if (null? set)
      #f
      (if (equal? element (car set))
          #t
          (memq? element (cdr set)))))

(define (encode-symbol symbol tree)
  (if (and (leaf? tree)
           (eq? (symbol-leaf tree) symbol))
      '()
      (if (memq? symbol (symbols tree))
          (let ((leftb (left-branch tree))
                (rightb (right-branch tree)))
            (cond ((memq? symbol (symbols leftb))
                   (cons 0 (encode-symbol symbol leftb)))
                  ((memq? symbol (symbols rightb))
                   (cons 1 (encode-symbol symbol rightb)))
                  (else (error "malformed tree -- ENCODE-SYMBOL"))))
          (error "symbol not in tree -- ENCODE-SYMBOL" symbol))))

;;; Exercise 2.69
;;; =============

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge trees)
  (if (empty? (cdr trees))
      (car trees)
      (successive-merge
       (adjoin-set (make-code-tree (car trees)
                                   (cadr trees))
                   (cddr trees)))))

;;; Exercise 2.70
;;; =============

(define nana-tree
  (generate-huffman-tree '((A 2) (BOOM 1) (GET 2)
                           (JOB 2) (NA 16) (SHA 3)
                           (YIP 9) (WAH 1))))

(define nana-message
  (encode '(GET A JOB
                SHA NA NA NA NA NA NA NA NA
                GET A JOB
                SHA NA NA NA NA NA NA NA NA
                WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                SHA BOOM)
          nana-tree))

;; 84 bits are required for the encoding.
;; For a fixed length encoding:
;; 36 symbols * 3 bits/symbol = 108 bits

;;; Exercise 2.71
;;; =============

(define tree5
  (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))

(define tree10
  (generate-huffman-tree
   '((A 1) (B 2) (C 4) (D 8) (E 16)
     (F 32) (G 64) (H 128) (I 256) (J 512))))

;; In general, 1 bit is required for the most frequent symbol,
;; (n-1) bits for the least frequent.

;;; Exercise 2.72
;;; =============

;; In the worst case, O(n^2).  This is the case for the least
;; frequent symbols in such an alphabet as in Ex 2.71.  For the
;; most frequent symbol, it is O(n) in the worst case because
;; of the search through the symbol set.
