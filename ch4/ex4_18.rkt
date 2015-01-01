;;; Ex 4.18

;;; New strategy

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;;; -->

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

;;; -->

(define (solve f y0 dt)
  ((lambda (y dy)
     ((lambda (a b)
        (set! y a)
        (set! dy b))
      (integral (delay dy) y0 dt)
      (stream-map f y))
     y)
   '*unassigned* '*unassigned*))

;;; At the point (stream-map f y) is evaluated for input to the inner
;;; lambda, y is *unassigned* and thus attempting to use its value causes
;;; an error.

;;; Original scanning out

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;;; -->

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

;;; -->

(define (solve f y0 dt)
  ((lambda (y dy)
     (set! y (integral (delay dy) y0 dt))
     (set! dy (stream-map f y))
     y)
   '*unassigned* '*unassigned*))

;;; This works fine, because at the time y is set, the value of dy is
;;; delayed (not evaluated), and at the time dy is set, y has already
;;; been set.
