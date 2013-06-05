#lang racket

(require "ass2.rkt" "equation-adt.rkt" "substitution-adt.rkt" "type-expression-adt.rkt")
(provide (all-defined-out))

;;; Infer type of a Scheme expression:

;Signature: infer-type(scheme-expr)
;Type: Client view: [Scheme-expression -> Type-expression]
;      Implmentation view: [LIST union Symbol union Number -> LIST union Symbol]
;Purpose: Infer the type of a scheme expression using the equations method
;Tests: (infer-type '(lambda (f x) (f (f x)))) ==> (-> (* (-> (* T_4) T_4) T_4) T_4)
(define infer-type 
  (lambda (scheme-expr)
    (let ((expr-tvars-list (make-expr-tvars-list scheme-expr)))  
      (get-expression-of-variable
        (solve-equations (make-equations scheme-expr expr-tvars-list))
        (get-var-of-expr expr-tvars-list scheme-expr))
      )
  ))  

;Signature: solve-equations(equation-list)
;Type: Client view: [List(Equation) -> Substitution]
;      Implmentation view: [LIST -> LIST]
;Purpose: Solve the type equations and return the resulting substitution or error, if not solvable
;Tests: (solve-equations
;            (make-equations 
;                    '((lambda(x)(x 11)) (lambda(y) y)) 
;                    (make-expr-tvars-list '((lambda(x)(x 11)) (lambda(y) y))))
;                 ==>
;    '((T_59 T_63 T_57 T_61 T_60 T_62 T_58) 
;      (Number Number Number Number 
;        (-> (* Number) Number) 
;        (-> (* Number) Number) 
;        (-> (* (-> (* Number) Number)) Number)))
(define solve-equations 
  (lambda(equations)
    (solve equations (make-sub '() '()))))


;Signature: solve(equations,substitution)
;Type: Client view: [List(Equation)*Substitution -> Substitution]
;      Implmentation view: [LIST -> LIST]
;Purpose: Solve the equations, starting from a given substitution. return the resulting substitution, or error, if not solvable
;Tests: (solve (make-equations 
;                    '((lambda(x)(x 11)) (lambda(y) y)) 
;                    (make-expr-tvars-list '((lambda(x)(x 11)) (lambda(y) y))))
;                (make-sub '() '())) ==>
;    '((T_59 T_63 T_57 T_61 T_60 T_62 T_58) 
;      (Number Number Number Number 
;       (-> (* Number) Number) 
;       (-> (* Number) Number) 
;       (-> (* (-> (* Number) Number)) Number)))
(define solve 
  (lambda(equations subst)
    (if (null? equations)
        subst    
        (let ((eq (make-equation-from-tes (substitution-application subst (get-left (get-first-equation equations))) 
                                          (substitution-application subst (get-right (get-first-equation equations)))))
             )
          (letrec ((solve-var-eq            ;If one side of eq is a variable 
                    (lambda(var-part other-part) 
                         (solve (cdr equations) 
                                (substitution-combination subst (make-sub (list var-part) (list other-part))))))
                   (both-sides-atomic? 
                    (lambda(eq) (and (atomic? (get-left eq)) (atomic? (get-right eq)))))
                   (handle-both-sides-atomic 
                    (lambda(eq) (if (equal-atomic-te? (get-left eq) (get-right eq)) 
                                    (solve (get-rest-equations equations) subst)
                                    (error 'solve "equation contains unequal atomic types: ~e" eq))))
                  )
            (cond ((variable? (get-left eq)) (solve-var-eq (get-left eq) (get-right eq)))              
                  ((variable? (get-right eq)) (solve-var-eq (get-right eq) (get-left eq))) 
                  ((both-sides-atomic? eq) (handle-both-sides-atomic eq))
                  ((and (composite? (get-left eq)) (composite? (get-right eq)) (unifyable-structure eq)) 
                   (solve (cons (get-rest-equations equations) (split-equation eq)) subst))
                  (else (error 'solve "equation contains unknown type expresion: ~e" eq)))
           )))
  ))

;Signature: unifyable-structure(equation)
;Type: Client view: [Equation -> Boolean]
;      Implementation view: [LIST -> Boolean]
;Purpose: Compars the structure of the type expressions of the equation
;Tests: (unifyable-structure '(((-> (* T_3) T_1)) ((-> (* T_2) T_1)))) ==> #t
;         (unifyable-structure '(T_0 (-> (* T_3) T_1)) ==> #f
(define unifyable-structure
  (lambda(eq)
   (let ((left (get-left eq))
         (right (get-right eq)))
     (or (and (procedure? left) (procedure? right) 
              (= (tuple-length (proc-parameter-tuple-tes left)) 
                 (tuple-length (proc-parameter-tuple-tes right))))
         (and (tuple? left) (tuple? right)
              (= (tuple-length left) (tuple-length right))))
     ) 
  ))


;Signature: splitter(equation)
;Type: Client view: [Equation -> List(Equation)]
;      Implementation view: [LIST -> LIST]
;Purpose: For an equation with unifyable type expressions, create equations for corresponding components.
;Tests: (splitter (make-equation-from-tes '(-> (* T_3) (-> (* T_3) T_1)) 
;                                      '(-> (* T_3) (-> (* T_2) T_2)))) ==>
;==> ( ( (-> (* T_3) T_1) (-> (* T_2) T_2)) (T_3 T_3))
;Pre-condition: (and (composite? (get-left eq)) (composite? (get-right eq)) (unifyable-structure eq)) = #t

(define splitter 
  (lambda(eq)
   (let((left-eq (get-left eq)) (right-eq (get-right eq)))
    (cond  
      ((variable? left-eq) (list left-eq right-eq))
      ((atomic? left-eq) (list left-eq right-eq))
      ((tuple? left-eq) (let 	((left-return-te (tuple-components left-eq))
					 (right-return-te (tuple-components right-eq)))
				 (if (or (null? left-return-te)  (null? right-return-te))
					(list) 
					(append (splitter(make-equation-from-tes (car  left-return-te) (car  right-return-te)))
					      (splitter(make-equation-from-tes (make-tuple-te(cdr  left-return-te))
                                                                                     (make-tuple-te(cdr  right-return-te)))) )
								 )))
      ((procedure? left-eq) (let 	((left-return-te (proc-return-te left-eq))
								 (right-return-te (proc-return-te right-eq))
								 (left-return-tuple ( proc-parameter-tuple-tes left-eq))
								(right-return-tuple ( proc-parameter-tuple-tes right-eq)))
								 (append (splitter(make-equation-from-tes left-return-te right-return-te ))
									   (splitter(make-equation-from-tes left-return-tuple right-return-tuple ))
								 )
						))))))


;Signature: split-equation(equation)
;Type: Client view: [Equation -> List(Equation)]
;      Implementation view: [LIST -> LIST]
;Purpose: For an equation with unifyable type expressions, create equations for corresponding components.
;Tests: (split-equation (make-equation-from-tes '(-> (* T_3) (-> (* T_3) T_1)) 
;                                      '(-> (* T_3) (-> (* T_2) T_2)))) ==>
;==> ( ( (-> (* T_3) T_1) (-> (* T_2) T_2)) (T_3 T_3))
;Pre-condition: (and (composite? (get-left eq)) (composite? (get-right eq)) (unifyable-structure eq)) = #t
(define split-equation 
  (lambda(eq)
    (accumulate-two-arguments (splitter eq))))
    
    
 
;Signature: accumulate-two-arguments(equation)
;Type: Client view: [Equation -> List(Equation)]
;      Implementation view: [LIST -> LIST]
;Purpose: For an equation with unifyable type expressions, create equations for corresponding components.
;Tests: (accumulate-two-arguments ('1 2 )) ==> '((1 2))
;Pre-condition:
(define accumulate-two-arguments
  (lambda(lst)
    (if (or (null? lst) (= (length lst) 1)) lst
        (cons (list (car lst) (car (cdr lst))) (accumulate-two-arguments (cdr (cdr lst))))
      )))