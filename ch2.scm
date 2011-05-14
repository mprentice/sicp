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
