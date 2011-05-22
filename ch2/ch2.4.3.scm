;;; =============
;;; Section 2.4.3
;;; =============

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;; Exercise 2.73
;;; =============

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; a. deriv now dispatches on the operator (+, *, etc) rather than
;;    containing the logic for each operator type.  We can't do this
;;    as written with numbers and variables because they are not in
;;    list structure, so don't have car and cdr.

;; b.
(define (install-deriv-package)
  (define (deriv+ operands var)
    (let ((exp (cons '+ operands)))
      (make-sum (deriv (addend exp) var)
                (deriv (augend exp) var))))
  (define (deriv* operands var)
    (let ((exp (cons '* operands)))
      (make-sum
       (make-product (multiplier exp)
                     (deriv (multiplicand exp) var))
       (make-product (deriv (multiplier exp) var)
                     (multiplicand exp)))))
  (put 'deriv '(+) deriv+)
  (put 'deriv '(*) deriv*)
  'done)

;; c.
(define (install-deriv-exponentiation)
  (define (deriv** operands var)
    (let ((exp (cons '** operands)))
      (make-product
       (make-product (exponent exp)
                     (make-exponentiation (base exp)
                                          (make-sum (exponent exp)
                                                    -1)))
       (deriv (base exp) var))))
  (put 'deriv '(**) deriv**)
  'done)

;; d. Only the put lines have to change in the installs.

;;; Exercise 2.74
;;; =============

;; a.
(define (get-record employee personnel)
  ((get 'get-record (list (division personnel))) employee personnel))
;; We need to be able to get the division from the personnel file.
;; (Or the employee parameter.)
;; (get 'get-record '(division)) should return the appropriate function
;; to search a division's personnel file for an employee.

;; b.
(define (get-salary employee)
  ((get 'get-salary
        (list (division employee)))
   (get-record employee (personnel-file (division employee)))))
;; We need to be able to get the division from the employee parameter.
;; We also need to be able to get the personnel file for a division.
;; Finally, (get 'get-salary '(division)) should return the appropriate
;; function to get an employee's salary from the record.

;; c.
(define (find-employee-record employee files)
  (if (empty? files)
      null
      (let ((record (get-record employee (car files))))
        (if (not (null? record))
            record
            (find-employee-record employee (cdr files))))))

;; d. The new division must define get-record, get-salary, and
;;    find-employee-record functions with put.

;;; Exercise 2.75
;;; =============

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;;; Exercise 2.76
;;; =============

;; Essay question.
