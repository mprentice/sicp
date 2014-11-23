#lang planet neil/sicp

;;; Metacircular evaluator from Chapter 4 of SICP

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
        ((application? exp) (apply (eval (operator exp) env)
                                   (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

;;; Apply a procedure to its arguments.
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else (error ("Unknown procedure type -- APPLY" procedure)))))

;;; Return eval applied to a list of expressions.
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;;; Exercise 4.1: list of values left to right order
(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (let ((rest (list-of-values-left-to-right (rest-operands exps) env)))
          (cons first rest)))))

;;; Exercise 4.1: list of values right to left order
(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values-right-to-left (rest-operands exps) env)))
        (let ((first (eval (first-operand exps) env)))
          (cons first rest)))))

;;; Evaluate an if expression in the given environment.
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;; Evaluate a sequence of expressions in the given environment and
;;; return the value of the last expression.
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
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

