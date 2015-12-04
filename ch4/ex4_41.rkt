#lang planet neil/sicp

;;; Exercise 4.41

(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (define (answer? baker cooper fletcher miller smith)
    (and (distinct? (list baker cooper fletcher miller smith))
	 (not (= baker 5))
	 (not (= cooper 1))
	 (not (= fletcher 5))
	 (not (= fletcher 1))
	 (> miller cooper)
	 (not (= (abs (- smith fletcher)) 1))
	 (not (= (abs (- fletcher cooper)) 1))))
  (define (try-it baker cooper fletcher miller smith)
    (cond ((answer? baker cooper fletcher miller smith)
	   (list (list 'baker baker)
		 (list 'cooper cooper)
		 (list 'fletcher fletcher)
		 (list 'miller miller)
		 (list 'smith smith)))
	  ((< smith 5) (try-it baker cooper fletcher miller (+ smith 1)))
	  ((< miller 5) (try-it baker cooper fletcher (+ miller 1) 1))
	  ((< fletcher 5) (try-it baker cooper (+ fletcher 1) 1 1))
	  ((< cooper 5) (try-it baker (+ cooper 1) 1 1 1))
	  ((< baker 5) (try-it (+ baker 1) 1 1 1 1))
	  (else false)))
  (try-it 1 1 1 1 1))


