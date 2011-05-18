#lang racket/gui

(define (square x) (* x x))

;(define rogers-bitmap (read-bitmap "ch2-Z-G-30.gif"))
;(define rogers
;  (lambda (frame)
;    (lambda (bm)
;      (let* ((dc (new bitmap-dc% [bitmap bm]))
;             (w (send bm get-width))
;             (h (send bm get-height))
;             (origin (origin-frame frame))
;             (edge1 (edge1-frame frame))
;             (edge2 (edge2-frame frame))
;             (x1 (xcor-vect origin))
;             (y1 (ycor-vect origin))
;             (x2 (xcor-vect edge1))
;             (y2 (ycor-vect edge1))
;             (x3 (xcor-vect edge2))
;             (y3 (ycor-vect edge2))
;             (dx (inexact->exact (round (* x1 w))))
;             (dy (inexact->exact (round (* y1 h))))
;             (xscale (sqrt (+ (square (- x2 x1)) (square (- y2 y1)))))
;             (yscale (sqrt (+ (square (- x3 x1)) (square (- y3 y1)))))
;             (rot (atan (/ (- y2 y1) (- x2 x1)))))
;        (send dc scale (/ w (send rogers-bitmap get-width))
;              (/ h (send rogers-bitmap get-height)))
;        (send dc scale xscale yscale)
;        (send dc rotate rot)
;        (send dc translate dx dy)
;        (send dc draw-bitmap rogers-bitmap 0 0)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner1) new-origin)
                             (sub-vect (m corner2) new-origin)))))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (lambda (bitmap)
          ((paint-left frame) bitmap)
          ((paint-right frame) bitmap)
          bitmap)))))

;;; Exercise 2.51
;(define (below painter1 painter2)
;  (let ((split-point (make-vect 0.0 0.5)))
;    (let ((paint-bottom
;           (transform-painter painter1
;                             (make-vect 0.0 0.0)
;                             (make-vect 1.0 0.0)
;                             split-point))
;          (paint-top
;           (transform-painter painter2
;                              split-point
;                              (make-vect 1.0 0.5)
;                              (make-vect 0.0 1.0))))
;      (lambda (frame)
;        (lambda (bitmap)
;          ((paint-bottom frame) bitmap)
;          ((paint-top frame) bitmap))))))
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;;; Exercise 2.45
(define (split op1 op2)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  splitter)

(define right-split (split beside below))

;; Exercise 2.44
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;;; Exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;;; Exercise 2.50
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define flipped-pairs 
  (square-of-four identity flip-vert identity flip-vert))

;;; Exercise 2.46
(define (make-frame orig-vect edge1-vect edge2-vect)
  (list orig-vect edge1-vect edge2-vect))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2))
                                    (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2))
                                    (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v) (make-vect (* s (xcor-vect v))
                                    (* s (ycor-vect v))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;;; Exercise 2.48
(define (make-segment start-vect end-vect) (cons start-vect end-vect))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (segments->painter segment-list)
  (lambda (frame)
    (lambda (bm)
      (let ((dc (new bitmap-dc% [bitmap bm])))
        (for-each
         (lambda (segment)
           (let* ((w (send bm get-width))
                  (h (send bm get-height))
                  (start ((frame-coord-map frame) (start-segment segment)))
                  (end ((frame-coord-map frame) (end-segment segment)))
                  (x1 (inexact->exact (round (* w (xcor-vect start)))))
                  (x2 (inexact->exact (round (* w (xcor-vect end)))))
                  (y1 (inexact->exact (round (- h (* h (ycor-vect start))))))
                  (y2 (inexact->exact (round (- h (* h (ycor-vect end)))))))
             (send dc draw-line x1 y1 x2 y2)))
         segment-list)
        bm))))

;;; Exercise 2.49
(define outline
  (let ((bl (make-vect 0 0))
        (tl (make-vect 0 1))
        (tr (make-vect 1 1))
        (br (make-vect 1 0)))
    (let ((left (make-segment bl tl))
          (top (make-segment tl tr))
          (right (make-segment tr br))
          (bottom (make-segment br bl)))
      (segments->painter (list left top right bottom)))))
(define cross
  (let ((bl (make-vect 0 0))
        (tl (make-vect 0 1))
        (tr (make-vect 1 1))
        (br (make-vect 1 0)))
    (let ((diag1 (make-segment bl tr))
          (diag2 (make-segment tl br)))
      (segments->painter (list diag1 diag2)))))
(define diamond
  (let ((top (make-vect 0.5 1))
        (left (make-vect 0 0.5))
        (bottom (make-vect 0.5 0))
        (right (make-vect 1 0.5)))
    (let ((seg1 (make-segment top right))
          (seg2 (make-segment right bottom))
          (seg3 (make-segment bottom left))
          (seg4 (make-segment left top)))
      (segments->painter (list seg1 seg2 seg3 seg4)))))
(define wave
  (let ((a (make-vect 0.45 1))
        (b (make-vect 0.55 1))
        (c (make-vect 0 0.8))
        (d (make-vect 0.4 0.8))
        (e (make-vect 0.6 0.8))
        (f (make-vect 0 0.6))
        (g (make-vect 0.35 0.6))
        (h (make-vect 0.45 0.6))
        (i (make-vect 0.55 0.6))
        (j (make-vect 0.7 0.6))
        (k (make-vect 0.2 0.55))
        (l (make-vect 0.35 0.55))
        (m (make-vect 0.4 0.5))
        (n (make-vect 0.2 0.45))
        (o (make-vect 0.55 0.45))
        (p (make-vect 1 0.4))
        (q (make-vect 0.5 0.2))
        (r (make-vect 1 0.2))
        (s (make-vect 0.3 0))
        (t (make-vect 0.45 0))
        (u (make-vect 0.55 0))
        (v (make-vect 0.7 0)))
    (let ((ad (make-segment a d))
          (dh (make-segment d h))
          (hg (make-segment h g))
          (gk (make-segment g k))
          (kc (make-segment k c))
          (fn (make-segment f n))
          (nl (make-segment n l))
          (lm (make-segment l m))
          (ms (make-segment m s))
          (tq (make-segment t q))
          (qu (make-segment q u))
          (vo (make-segment v o))
          (or (make-segment o r))
          (pj (make-segment p j))
          (ji (make-segment j i))
          (ie (make-segment i e))
          (eb (make-segment e b)))
      (segments->painter (list ad dh hg gk kc
                               fn nl lm ms
                               tq qu
                               vo or
                               pj ji ie eb)))))

(define origin (make-frame (make-vect 0 0)
                           (make-vect 1 0)
                           (make-vect 0 1)))

(define (paint painter)
  (let* ((bitmap (make-bitmap 1024 1024))
         (frame (new frame% [label "Painting"]
                     [width 300]
                     [height 300]))
         (canvas (new canvas% [parent frame]
                      [paint-callback
                       (lambda (canvas dc)
                         (send dc erase)
                         (let ((cw (send canvas get-width))
                               (ch (send canvas get-height))
                               (w (send bitmap get-width))
                               (h (send bitmap get-height)))
                           (send dc set-scale (/ cw w) (/ ch h))
                           (send dc draw-bitmap bitmap 0 0)))])))
    ((painter origin) bitmap)
    (send frame show #t)))

(paint (square-limit wave 4))

;(define (rogers frame)
  ;; draw rogers onto frame
;  (draw-bitmap frame))

