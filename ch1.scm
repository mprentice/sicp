;; General helper functions
(define (square n)
  (* n n))

;;; Exercise 1.1
;;; ============
;;; A bunch ov evals

;;; Exercise 1.2
;;; =============
(define (ex1.2)
  (/ (+ 5 4 (- 2 (- 3 (+ 6 4/3))))
     (* 3 (- 6 2) (- 2 7))))

;;; Exercise 1.3
;;; ============
(define (ex1.3 x y z)
  (if (> x y)
      (+ (square x)
         (if (> z y)
             (square z)
             (square y)))
      (+ (square y)
         (if (> z x)
             (square z)
             (square x)))))

;;; Exercise 1.4
;;; ============
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;;; Exercise 1.5
;;; ============
;;; Applicative vs normal-order evaluation.  Infinite recursion.

;;; Exercise 1.6
;;; ============
;;; Infinite recursion.

;;; Exercise 1.7
;;; ============

(define (sqrt-iter guess x)
  (if (sqrt-good-enough? guess x)
      guess
      (sqrt-iter (sqrt-improve guess x)
                 x)))

(define (sqrt-good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter1.7 guess old-guess x)
  (if (good-enough1.7? guess old-guess)
      guess
      (sqrt-iter1.7 (sqrt-improve guess x)
                 guess
                 x)))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough1.7? guess old-guess)
  (< (abs (/ (- guess old-guess) guess)) 0.0001))

(define (sqrt1.7 x)
  (sqrt-iter1.7 1.0 x x))

;;; Exercise 1.8
;;; ============

(define (cubert-iter guess x)
  (if (cubert-good-enough? guess x)
      guess
      (cubert-iter (cubert-improve guess x)
                   x)))

(define (cubert-good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (cubert-improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cubert x)
  (cubert-iter 1.0 x))

;;; Exercise 1.9
;;; ============

(define (plusa a b)
  (if (= a 0)
      b
      (inc (plusa (dec a) b))))

(define (plusb a b)
  (if (= a 0)
      b
      (plusb (dec a) (inc b))))

;;; Exercise 1.10
;;; =============

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (ex1.10)
  (list (A 1 10)
        (A 2 4)
        (A 3 3)))

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

;;; Exercise 1.11
;;; =============

(define (f1 n)
  (if (< n 3)
      n
      (+ (f1 (- n 1))
         (* 2 (f1 (- n 2)))
         (* 3 (f1 (- n 3))))))

(define (f2 n)
  (define (f2-iter a b c count)
    (cond
     ((= count 0) c)
     ((= count 1) b)
     ((= count 2) a)
     (else (f2-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f2-iter 2 1 0 n))

;;; Exercise 1.12
;;; =============
(define (pascal row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pascal (- row 1) (- col 1))
         (pascal (- row 1) col))))

;;; Exercise 1.13
;;; =============
;;; Mathy!

;;; Exercise 1.16
;;; =============
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (fast-expt1 b n)
  ;; invariant: a * b^n remains constant
  (define (fast-expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter a (square b) (/ n 2)))
          (else (fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter 1 b n))

;;; Exercise 1.17
;;; =============
(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (fast-* a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-* (double a) (halve b)))
        (else (+ a (fast-* a (- b 1))))))

;;; Exercise 1.18
;;; =============
(define (fast-*1 a b)
  ;; invariant: acc + a * b remains constant
  (define (fast-*-iter acc a b)
    (cond ((= b 0) acc)
          ((even? b) (fast-*-iter acc (double a) (halve b)))
          (else (fast-*-iter (+ acc a) a (- b 1)))))
  (fast-*-iter 0 a b))

;;; Exercise 1.19
;;; =============

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   <??>
                   <??>
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
