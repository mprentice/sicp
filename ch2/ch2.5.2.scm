;;; =============
;;; Section 2.5.2
;;; =============

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;; Exercise 2.81
;;; =============

;; a. Infinite loop.
;; b. apply-generic works as-is
;; c.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2)
                   (not (eq? (car type-tags) (cadr type-tags))))
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;; Exercise 2.82
;;; =============

;; Consider: (f [A val1] [B val2])
;; Assume f: B x B -> B exists, but no f: A x A -> ? exists.
;; Coercions: A->B, B->A
;; Then apply-generic will always try to coerce all values to type A, and will
;; will never explore the option A->B that would allow f to be calculated.
;; This is a problem in apply-generic as given, but in reverse (swap types A
;; and B).
;; Unfortunately, we have to explore the entire coercion space, keeping track
;; of which coercions have been tried so we don't go in a circle.

;;; Exercise 2.83
;;; =============

(define (raise n) (apply-generic 'raise n))

(put 'raise '(integer) integer->rational)
(put 'raise '(rational) rational->real)
(put 'raise '(real) real->complex)

;;; Exercise 2.84
;;; =============

;;; Exercise 2.85
;;; =============

;;; Exercise 2.86
;;; =============

