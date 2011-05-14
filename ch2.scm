;;; Exercise 2.1
;;; ============

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let* ([np (abs n)]
         [dp (abs d)]
         [g (gcd np dp)]
         [num (/ np g)]
         [den (/ dp g)])
    (if (not (eq? (positive? n)
                  (positive? d)))
        (cons (- num) den)
        (cons num den))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

;;; Exercise 2.2
;;; ============

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg))
                    (x-point (end-segment seg)))
                 2)
              (/ (+ (y-point (start-segment seg))
                    (y-point (end-segment seg)))
                 2)))

;;; Exercise 2.3
;;; ============

(define (make-rect p1 p2)
  (let ((x1 (x-point p1))
        (x2 (x-point p2))
        (y1 (y-point p1))
        (y2 (y-point p2)))
    (list (make-point (if (< x1 x2) x1 x2)
                      (if (< y1 y2) y1 y2))
          (make-point (if (< x1 x2) x1 x2)
                      (if (< y1 y2) y2 y1))
          (make-point (if (< x1 x2) x2 x1)
                      (if (< y1 y2) y2 y1))
          (make-point (if (< x1 x2) x2 x1)
                      (if (< y1 y2) y1 y2)))))

(define (ll-rect r) (first r))
(define (ul-rect r) (second r))
(define (ur-rect r) (third r))
(define (lr-rect r) (fourth r))
(define (height-rect r) (- (y-point (ul-rect r)) (y-point (ll-rect r))))
(define (width-rect r) (- (x-point (lr-rect r)) (x-point (ll-rect r))))
(define (perim-rect r) (+ (* (width-rect r) 2) (* (height-rect r) 2)))
(define (area-rect r) (* (width-rect r) (height-rect r)))

;; Another way to represent it would be to only store the top-left
;; and bottom-right points, and calculate bottom-left and upper-right
;; as needed.

;;; Exercise 2.4
;;; ============

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;;; Exercise 2.5
;;; ============

(define (cons a b)
  (define (cons-iter a b acc)
    (if (< a 1)
        (if (< b 1)
            acc
            (cons-iter 0 (- b 1) (* acc 3)))
        (cons-iter (- a 1) b (* acc 2))))
  (cons-iter a b 1))
(define (car c) (if (= (modulo c 2) 0) (+ 1 (car (/ c 2))) 0))
(define (cdr c) (if (= (modulo c 3) 0) (+ 1 (cdr (/ c 3))) 0))

;;; Exercise 2.6
;;; ============

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
  (add-1 zero))

(define one
  (lambda (f) (lambda (x) (f ((zero f) x)))))

(define one
  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))

(define two
  (add-1 one))

(define two
  (lambda (f) (lambda (x) (f ((one f) x)))))

(define two
  (lambda (f)
    (lambda (x)
      (f (((lambda (f)
             (lambda (x)
               (f (((lambda (f)
                      (lambda (x) x)) f) x)))) f) x)))))

(define (+ a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

;;; Exercise 2.7
;;; ============

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

;;; Exercise 2.8
;;; ============

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;; Exercise 2.9
;;; ============

(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))

;; x = (x1,x2), y = (y1,y2)
;; width(x+y) = (upper(x+y) - lower(x+y)) / 2
;;            = (upper((x1+y1),(x2+y2)) - lower((x1+y1),(x2+y2))) / 2
;;            = ((x2+y2) - (x1+y1)) / 2
;;            = (x2+y2)/2 - (x1+y1)/2
;;            = x2/2 + y2/2 - x1/2 - y1/2
;;            = x2/2 - x1/2 + y2/2 - y1/2
;;            = (x2 - x1) / 2 + (y2 - y1) / 2
;;            = width(x) + width(y)
;; width(x-y) = (upper(x-y) - lower(x-y)) / 2
;;            = (upper((x1-y2),(x2-y1)) - lower((x1-y2),(x2-y1))) / 2
;;            = ((x2-y1) - (x1-y2)) / 2
;;            = (x2 - y1 - x1 + y2) / 2
;;            = (x2 - x1 + y2 - y1) / 2
;;            = (x2 - x1) / 2 + (y2 - y1) / 2
;;            = width(x) + width(y)
;; Consider x = (-1,1), y = (5,15):
;;   width(x) = 1, width(y) = 5
;;   width(x*y) = 15
;; Now consider x = (1,3), y = (5,15):
;;   width(x) = 1, width(y) = 5, as before.
;;   width(x*y) = 20

;;; Exercise 2.10
;;; =============

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0)
           (>= (upper-bound y) 0))
      (error "Cannot divide by an interval that spans zero.")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;;; Exercise 2.11
;;; =============

(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((and (positive? x1) (positive? y1)) (make-interval (* x1 y1)
                                                              (* x2 y2)))
          ((and (positive? x1) (negative? y2)) (make-interval (* x2 y1)
                                                              (* x1 y2)))
          ((and (negative? x2) (positive? y1)) (make-interval (* x1 y2)
                                                              (* x2 y1)))
          ((and (negative? x2) (negative? y2)) (make-interval (* x2 y2)
                                                              (* x1 y1)))
          ((and (negative? x1)
                (positive? x2)
                (positive? y1)) (make-interval (* x1 y2) (* x2 y2)))
          ((and (negative? x1)
                (positive? x2)
                (negative? y2)) (make-interval (* x2 y1) (* x1 y1)))
          ((and (positive? x1)
                (negative? y1)
                (positive? y2)) (make-interval (* x2 y1) (* x2 y2)))
          ((and (negative? x2)
                (negative? y1)
                (positive? y2)) (make-interval (* x1 y2) (* x1 y1)))
          (else 
           (let ((p1 (* (lower-bound x) (lower-bound y)))
                 (p2 (* (lower-bound x) (upper-bound y)))
                 (p3 (* (upper-bound x) (lower-bound y)))
                 (p4 (* (upper-bound x) (upper-bound y))))
             (make-interval (min p1 p2 p3 p4)
                            (max p1 p2 p3 p4)))))))

;;; Exercise 2.12
;;; =============

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (let ((c (center i)))
    (* (/ (- c (lower-bound i)) c) 100)))

;;; Exercise 2.13
;;; =============

;; Mathy.

;;; Exercise 2.14
;;; =============

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; TODO: Mathy

;;; Exercise 2.15
;;; =============

;; Yes, because error bounds can never decrease.

;;; Exercise 2.16
;;; =============

;; In general, impossible with floating point arithmetic.
;; I think it can be done in a symbolic arithmetic package.
