;; General helper functions
(define (square n)
  (* n n))

(define (cube n)
  (* n n n))

(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

(define (identity n)
  n)

;;; Exercise 1.1
;;; ============
;;; A bunch of evals

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
(define (fast-expt-r b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
(define (fast-expt b n)
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
                   (+ (* p p) (* q q))   ; p' = p^2 + q^2
                   (+ (* q q) (* 2 p q)) ; q' = q^2 + 2pq
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;; Exercise 1.22
;;; =============

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define runtime current-milliseconds) ;;; for running in Racket

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      null))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (cond ((even? start) (search-for-primes (+ start 1) end))
        ((even? end) (search-for-primes start (- end 1)))
        ((= start end) (timed-prime-test end))
        (else (timed-prime-test start)
              (search-for-primes (+ start 2) end))))
  
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;; Exercise 1.27
;;; =============

(define (carmichael? n)
  (define (carmichael-iter a n)
    (cond ((= a n) true)
          ((= (expmod a n n) a) (carmichael-iter (+ 1 a) n))
          (else false)))
  (and (carmichael-iter 2 n) (not (prime? n))))

;;; Exercise 1.28
;;; =============

(define (expmod-1.28 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (and (not (or (= base 1)   ; a nontrivial square root of 1 mod m
                           (= base (- m 1))))
                  (= (remainder (square base) m) 1))
             0                          ; return 0 to signal nontrivial sqrt
             (remainder (square (expmod-1.28 base (/ exp 2) m))
                        m)))
        (else (remainder (* base (expmod-1.28 base (- exp 1) m))
                         m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (not (= (expmod-1.28 a (- n 1) n) 0)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-1.28? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime-1.28? n (- times 1)))
        (else false)))

;;; Exercise 1.29
;;; =============

;; Use Simpson's Rule to compute the integral of f over [a,b] with
;; number of steps n.  n must be even.
(define (sr-integral f a b n)
  (let ((h (/ (- b a) n))
        (y0 (f a))
        (yn (f b)))
    (define (sr-integral-iter k acc)
      (if (= k n)
          acc
          (let ((yk (f (+ a (* k h)))))
            (sr-integral-iter (+ k 1)
                              (+ acc
                                 (if (odd? k)
                                     (* 4 yk)
                                     (* 2 yk)))))))
    (* (/ h 3) (+ y0 yn (sr-integral-iter 1 0)))))

;;; Exercise 1.30
;;; =============

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;; Exercise 1.31
;;; =============

;; a. recursively
(define (product-r term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-r term (next a) next b))))

;; b. iteratively
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (approx-pi n)
  (define (frac a)
    (if (even? a)
        (/ (+ a 2) (+ a 1))
        (/ (+ a 1) (+ a 2))))
  (* 4 (product frac 1 inc n)))

;;; Exercise 1.32
;;; =============

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;; Exercise 1.33
;;; =============

(define (filtered-accumulate combiner predicate? null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (predicate? a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))

(define (sum-sq-prime a b)
  (filtered-accumulate + prime? 0 square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

(define (ex1.33b n)
  (define (rel-prime? a)
    (= 1 (gcd a n)))
  (filtered-accumulate * rel-prime? 1 identity 2 inc (- n 1)))

;;; Exercise 1.34
;;; =============

;; (f f) gives an error, because (f f) --> (f 2) --> (2 2) gives an error

;;; Exercise 1.35
;;; =============

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

;;; Exercise 1.36
;;; =============

(define tolerance 0.00001)
(define (fixed-point-1.36 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "Guess: ")
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; Without average damping: 34 steps
;; With average damping: 9 steps

;;; Exercise 1.37
;;; =============

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (dec i)
              (/ (n i)
                 (+ (d i) result)))))
  (iter k 0))

;; k = 12
;; b. I don't want to write recursively. Much easier to write iteratively.

;;; Exercise 1.38
;;; =============

(define (euler-e k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= (modulo i 3) 2)
                      (* 2 (+ 1 (quotient i 3)))
                      1))
                k)))

;;; Exercise 1.39
;;; =============

(define (tan-cf x k)
  (let ((x2 (square x)))
    (cont-frac (lambda (i)
                 (if (= i 1)
                     x
                     x2))
               (lambda (i)
                 (- (* 2 i) 1))
               k)))

;;; Exercise 1.40
;;; =============

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;;; Exercise 1.41
;;; =============

(define (double f)
  (lambda (x)
    (f (f x))))

;; (((double (double double)) inc) 5)
;; --> 21

;;; Exercise 1.42
;;; =============

(define (compose f g)
  (lambda (x)
    (f (g x))))

;;; Exercise 1.43
;;; =============

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (dec n)))))

;;; Exercise 1.44
;;; =============

(define dx 0.0001)
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;;; Exercise 1.45
;;; =============

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root n x)
  (fixed-point ((repeated average-damp (floor (log2 n)))
                (lambda (y)
                   (/ x (expt y (- n 1)))))
               1.0))

;;; Exercise 1.46
;;; =============

(define (iterative-improve good-enough? improve)
  (lambda (x)
    (define (try guess)
      (if (good-enough? guess)
          guess
          (try (improve guess))))
    (try x)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (iterative-improve (lambda (x)
                       (< (abs (- x (f x))) tolerance))
                     (lambda (x)
                       (f x))))

(define (sqrt-1.46 x)
  (define (good-enough? guess)
    (< (abs (- (square guess)
               x))
       0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

