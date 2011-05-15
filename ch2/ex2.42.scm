;;; Exercise 2.42
;;; =============

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position r k q) (cons r q))
(define empty-board '())
(define (safe? k p)
  (define (on-diag? piece positions dir)
    (if (empty? positions)
        #f
        (or (= (+ piece dir) (car positions))
            (on-diag? (+ piece dir) (cdr positions) dir))))
  (let ((piece (car p))
        (positions (cdr p)))
    (and (empty? (filter (lambda (x) (= piece x)) positions))
         (not (on-diag? piece positions 1))
         (not (on-diag? piece positions -1)))))

(define (for-each f l)
  (if (empty? l)
      null
      (begin (f (car l))
             (for-each f (cdr l)))))

(define (draw-board b)
  (define (draw-line)
    (display "+")
    (for-each (lambda (x) (display "-"))
              (enumerate-interval 1 (- (* 2 (length b)) 1)))
    (display "+")
    (newline))
  (define (draw-cell r c)
    (if (= r c) "Q|" " |"))
  (define (draw-row row)
    (display "|")
    (for-each (lambda (pos) (display (draw-cell row pos))) b)
    (newline))
  (draw-line)
  (for-each draw-row (enumerate-interval 1 (length b)))
  (draw-line))
