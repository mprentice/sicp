;;; Ex 4.26

;;; Ben: implement as a special form.

(define (eval exp env)
  (cond
   ...
   ((unless? exp) (eval (unless->if exp) env))
   ...))

(define (unless? exp)
  (tagged-list? exp 'unless))

(define (unless-condition exp) (cadr exp))

(define (unless-usual-value exp) (caddr exp))

(define (unless-exceptional-value exp) (cadddr exp))

(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))

;;; Alyssa: Since unless is implemented as a special form in eval, it
;;; isn't a procedure and can't be passed around like a normal
;;; procedure name bound to a lambda.
;;;
;;; We might want to pass unless or if as different strategies to a
;;; branching procedure, for example. It could change whether, e.g.
;;; when traversing a tree we traverse left-right or right-left. This
;;; can be approached other ways, of course, but the point is we can't
;;; do it as if unless and if are procedures because they are special
;;; forms in applicative-order languages.
