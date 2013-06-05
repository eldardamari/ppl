#lang racket

(require "asp.rkt" "type-expression-adt.rkt" "ass2.rkt")
(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equation ADT
; An equation is implemented as a list of two type expressions.


;Signature: make-equation-from tes(type-expr-l,type-expr-r)
;Type: Client view: [Type-expression*Type-expression -> Equation]
;      Implementation view: [LIST union Symbol * LIST union Symbol -> Tagged-data([Symbol->LIST union Symbol])]
;Purpose: equation constructor
;Tests: (make-equation-from-tes (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number) (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean)) ==> 
;                                   '((-> (* Number Number) Number) (-> (* Number Number) Boolean))
(define make-equation-from-tes
  (lambda (type-expr-l type-expr-r) 
    (attach-tag (lambda (m)
                  (cond ((eq? m 'left) type-expr-l)
                        ((eq? m 'right) type-expr-r)
                        (else error m "Expected left or right")))
                  'Equation)))

;Signature: make-empty-equation()
;Type: [Empty -> Tagged-data(LIST)]
;Purpose: Empty equation constructor
;Tests: (make-empty-equation)  ==> '() 
(define make-empty-equation
  (lambda () 
    (attach-tag (list) 'Equation)))

;Signature: get-left(eq)
;Type: Client view: [Equation -> type-expression]
;      Implementation view: [Tagged-data([Symbol->LIST union Symbol]) -> LIST union Symbol]
;Purpose: Returns left expression
;Tests: (get-left (make-equation-from-tes (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number) (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean)))
;                                        ==> '(-> (* Number Number) Number)
;Pre-condition: eq is a pair
(define get-left
  (lambda (eq) 
    (if (not (empty-equation? eq)) ((get-content eq) 'left) 'Empty)))


;Signature: get-right(eq)
;Type: Client view: [Equation -> type-expression]
;      Implementation view: [Tagged-data([Symbol->LIST union Symbol]) -> LIST union Symbol]
;Purpose: Returns left expression
;Tests: (get-right(make-equation-from-tes (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number) (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean)))
;                                           ==> '(-> (* Number Number) Boolean)
;Pre-condition: eq is a pair
(define get-right
  (lambda (eq)
    (if (not (empty-equation? eq)) ((get-content eq) 'right) 'Empty)))


;Signature: equation?(eq)
;Purpose: Returns true if param is an equation
;Type: [T -> Boolean]
;Tests: (equation? (make-equation-from-tes (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number) (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean)))
;           ==> t
(define equation?
  (lambda (eq)
    (tagged-by? eq 'Equation)))

;Signature: empty-equation?(eq)
;Purpose: Returns true if param is an empty equation
;Type: [Tagged-data([Symbol->LIST union Symbol])->Boolean]
;Tests: (empty-equation? (make-empty-equation)) ==> #t
(define empty-equation?
  (lambda (eq) 
    (and (equation? eq) (null? (get-content eq)))))


;Signature: get-var-of-expr(expr-tvars-list expr)
;Type: [LIST(LIST(LIST union Symbol * Symbol)) * LIST union Symbol -> Symbol]
;Purpose: Find the type of a Scheme expression in a list of pairs of a Scheme-expression and a type vars
;Tests: (get-var-of-expr '(((lambda (x) (+ x 1)) T_0) ((+ x 1) T_1) (1 T_4) (x T_3) (+ T_2)) '+)  ==> T_2 
(define get-var-of-expr
  (lambda (expr-tvars-list expr)
    (let ((expr-pair (assoc expr expr-tvars-list)))
      (if (and expr-pair (not (null? expr-pair)))
          (cadr expr-pair)
          void))
    ))


(define binary-numeric-primitive-type (list (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number)))
(define binary-logical-primitive-type (list (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean)))
(define binary-primitive-types (list (list '+ binary-numeric-primitive-type) (list '- binary-numeric-primitive-type) (list '* binary-numeric-primitive-type)
                                     (list '> binary-logical-primitive-type)))


;Signature: primitive-procedure?(se)
;Type: (LIST union Symbol -> Boolean]
;Purpose: returns if the procedure is primitive
;Tests: (primitive-procedure? '+)  ==> #t 
(define (primitive-procedure? se) 
  (if (or (not (symbol? se))
          (null? (filter (lambda(x) (and (list? x)(eq? (car x) se))) binary-primitive-types)))
         #f
         #t
     )
  )


;Signature: get-primitive-type(se)
;Type: (LIST union Symbol -> Boolean]
;Purpose: returns all primitive types
;Tests: (get-primitive-type '+)  ==> '(-> (* 'Number 'Number) 'Number)]
(define (get-primitive-type se)
  (let ((filtered (filter (lambda(x) (and (list? x)(eq? (car x) se))) binary-primitive-types)))
    (if (null? filtered) (list)
        (caadar filtered)
    )
  ))


;Signature:  make-equations(Scheme-expression,expression-type-vars-list)
;Purpose: Return a set of equations for a given Scheme expression and a list of pairs: Sub-expression and its type variable
;Type: Client view: [Scheme-expression*LIST(PAIR-LIST(Scheme-expression,Symbol)) -> LIST(Equation)]
;      Implementation view: [LIST union Symbol * LIST(LIST(LIST union Symbol * Symbol)) -> 
;                                                                 LIST(LIST(LIST union Symbol * LIST union Symbol))]
;Tests: (make-equations '(lambda (x) (+ x 1)) '(((lambda (x) (+ x 1)) var_0) ((+ x 1) var_1) (1 var_4) (x var_3) (+ var_2))) 
;    '((var_0 (-> (* var_3) var_1)) (var_2 (-> (* var_3 var_4) var_1)) (var_4 Number) (var_2 (-> (* Number Number) Number)))
(define make-equations
  (lambda (expr expr-tvars-list)    
      (filter (lambda(x) (not (null? x))) 
            (map (lambda (expr) (make-equation expr expr-tvars-list)) 
                 (map car expr-tvars-list)))))

;Signature: make-equation(Scheme-expression,expression-type-vars-list)
;Purpose: Return a single equation
;Type: Client view: [Scheme-expression*LIST(PAIR-LIST(Scheme-expression,Symbol)) -> LIST(Equation)]
;      Implementation view: [LIST union Symbol * LIST(LIST(LIST union Symbol * Symbol)) -> 
;                                                                    LIST(LIST union Symbol * LIST union Symbol)]
;Tests: (make-equation '(lambda (x) (+ x 1)) '(((lambda (x) (+ x 1)) var_0) ((+ x 1) var_1) (1 var_4) (x var_3) (+ var_2))) 
;    '(var_0 (-> (* var_3) var_1))
(define make-equation
  (lambda (se expr-tvars-list)
    (letrec ((get-var-of-expr 
                (lambda (expr)
                  (let ((expr-pair (assoc expr expr-tvars-list)))
                    (if (and expr-pair (not (null? expr-pair)))
                        (cadr expr-pair)
                        void))
                  )))
      (cond ((lambda? se)
             (let ((left (get-var-of-expr se))
                   (right (make-proc-te
                           (make-tuple-te
                            (map get-var-of-expr (lambda-parameters se)))
                           (get-var-of-expr (lambda-body-last-expr se))) )
                   )
                  (make-equation-from-tes left right)))
                   ((application? se)
                    (let ((left (get-var-of-expr (operator se)))
                          (right (make-proc-te
                                  (make-tuple-te
                                   (map get-var-of-expr (operands se)))
                                  (get-var-of-expr se)) )
                          )
                      (make-equation-from-tes left right))) 
                   ((number? se) 
                    (let ((left (get-var-of-expr se))
                          (right 'Number))
                      (make-equation-from-tes left right))) 
                   ((boolean? se) 
                    (let ((left (get-var-of-expr se))
                          (right 'Boolean))
                      (make-equation-from-tes left right)))
                   ((primitive-procedure? se) 
                    (let ((left (get-var-of-expr se))
                          (right (get-primitive-type se)))
                      (make-equation-from-tes left right)))
                   (else (make-empty-equation)))
      )
  ))
  
  
;Signature: get-first-equation(equations)
;Type: Client view: [LIST(Equation)->Scheme-expression]
;Purpose: Returns the first equation
;Test:
(define get-first-equation
  (lambda (equations) (car equations)))

;Signature: get-rest-equations(equations)
;Type: [LIST(Equation)->LIST(PAIR-LIST(Scheme-expression,Symbol))]
;Purpose: Returns the rest of the equations
;Test:
(define get-rest-equations
  (lambda (equations) (cdr equations)))
