#lang planet neil/sicp

;;; Metacircular evaluator from Chapter 4 of SICP

;;; (define apply-in-underlying-scheme apply)

;;; 4.2.2 An Interpeter with Lazy Evaluation

;;; Evaluate an expression in a given environment.
;;; Environment is a map of names to values.
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                  (lambda-body exp)
                                  env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (lazy-apply (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else (error "Unknown expression type -- EVAL" exp))))

;;; Get the actual value of an expression in an environment.
(define (actual-value exp env)
  (force-it (eval exp env)))

;;; Apply a procedure to its arguments. For primitive procedures,
;;; which are strict, we evaluate all arguments before applying the
;;; primitive. For compound procedures, which are non-strict, we delay
;;; all the arguments.
(define (lazy-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure
                                    (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            (list-of-delayed-args arguments env)
                                            (procedure-environment procedure))))
        (else (error ("Unknown procedure type -- LAZY-APPLY" procedure)))))

;;; Use actual-value instead of eval.
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

;;; Delay arguments instead of processing them.
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

;;; Evaluate an if expression in the given environment. Use
;;; actual-value to get value of predicate in lazy interpreter.
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;; Representing thunks

;;; Original presented for clarity:
;;; (define (force-it obj)
;;;   (if (thunk? obj)
;;;       (actual-value (thunk-exp obj) (thunk-env obj))
;;;       obj))
;;;
;;; Memoized:
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           ;; Replace exp with its value and forget unneeded env
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

;;; Evaluate a sequence of expressions in the given environment and
;;; return the value of the last expression.
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;; Install a variable binding in the given environment.
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;;; Install a definition in the given environment.
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;; 4.1.2 Representing Expressions

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)               ; formal parameters
              (cddr exp))))             ; body

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;; 4.1.3 Evaluator Data Structures

(define (true? x) (not (eq? x false)))

(define (false? x) (eq? x false))

;;; See section 4.1.4 for apply-primitive-procedure,
;;; primitive-procedure?

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values) (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

;;; 4.1.4 Running the Evaluator as a Program

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; more primitives
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
	(list 'list list)
	(list 'newline newline)
	(list 'display display)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply
   (primitive-implementation proc) args))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  ;; "> " is a hack to get around Emacs' read-only input prompt
  (newline) (display string) (newline) (display "> "))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; Exercise 4.30
;;; a.
;;;
;;; ;;; L-Eval input:
;;; > (define (for-each proc items)
;;;     (if (null? items)
;;; 	'done
;;; 	(begin (proc (car items))
;;; 	       (for-each proc (cdr items)))))
;;;
;;; ;;; L-Eval value:
;;; ok
;;; ;;; L-Eval input:
;;; > (for-each (lambda (x) (newline) (display x)) (list 57 321 88))
;;;
;;; 57
;;; 321
;;; 88
;;; ;;; L-Eval value:
;;; done
;;;
;;; Ben is right because eval-sequence calls eval on (newline) which
;;; will cause lazy-apply, and thus apply-primitive-procedure, to
;;; newline, which causes newline to be executed (the same would be
;;; true if we implemented newline in our lazy language and not as a
;;; primitive). Lazy evaluation simply means the arguments of newline
;;; won't be evaluated yet, but the call to newline has no
;;; arguments. The call itself happens and the procedure is executed.
;;;
;;; b.
;;;
;;; Original:
;;;
;;; ;;; L-Eval input:
;;; > (p1 1)
;;;
;;; ;;; L-Eval value:
;;; (1 2)
;;; ;;; L-Eval input:
;;; > (p2 1)
;;;
;;; ;;; L-Eval value:
;;; 1
;;;
;;; Cy's change:
;;;
;;; ;;; L-Eval input:
;;; > (p1 1)
;;;
;;; ;;; L-Eval value:
;;; (1 2)
;;; ;;; L-Eval input:
;;; > (p2 1)
;;;
;;; ;;; L-Eval value:
;;; (1 2)
;;;
;;; c. actual-value -> force-it -> eval -> lazy-apply ->
;;; primitive-procedure and then returns the result of applying
;;; primitive procedure, which is the newline. So the result is not
;;; affected by laziness vs non-laziness.
;;;
;;; d. I think Cy's approach jibes most with my intuition because I
;;; think of evaluating a sequence as evaluating every expression in
;;; that sequence in order, unlike lazy parameters in procedure
;;; application. But I'm tempted to simply leave side effects like
;;; that undefined when used with lazy evaluation. The coward's way
;;; out!