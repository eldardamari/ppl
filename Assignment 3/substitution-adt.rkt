#lang racket

(require "type-expression-adt.rkt")
(require srfi/1)
(provide (all-defined-out))


;;;;;;;;;;; Implementation of the Substitution ADT ;;;;;;;;;;;;;;;;;;
;A substitution is represented as a 2 element list of equal length lists of variables and
;type expression. The empty substitution is the list (() ()).

; Constructors:

;Signature: make-sub(variables, tes)
;Type: Client view: [LIST(Symbol)*LIST(Type-expression) -> Sunstitution]
;      Implementation view: [LIST(Symbol)*LIST(LIST union Symbol)) -> lIST] 
;Purpose: substitution constructor
;Tests: (make-sub '(x y z) 
;                (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number))) ==> 
;                                   ((x y z) (Number Boolean (-> (* Number) Number)))
;         (make-sub '(x y z) 
;                (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'z)) 'Number))) ==>
;     make-sub: circular substitution: ((x y z) (Number Boolean (-> (* z) Number)))
;         (make-sub '(x y z) (list 'Number 'x (make-proc-te (make-tuple-te (list 'y)) 'Number))) ==>
;                                   ((x y z) (Number x (-> (* y) Number)))
;Prec-condition: (length variables) = (length tes)
(define make-sub
  (lambda (variables tes)
    (if (fold (lambda (b1 b2) (and b1 b2)) #t (map non-circular? variables tes))
                                          ;if all variable substituting expressions are non-circular
        (list variables tes)
        (error 'make-sub "circular substitution: ~sub" (list variables tes)))
    ))

;Signature: non-circular?(var, te)
;Type: [Symbol*(LIST union Symbol) -> Boolean]
;Purpose: Check wheter susbstitutions is circular
;Tests: (non-circular? 'T (make-proc-te (make-tuple-te (list 'Number)) 'Number)) ==> #t
;         (non-circular? 'T (make-proc-te (make-tuple-te (list 'T)) 'Number)) ==> #f
(define non-circular?
  (lambda (var te)
    (cond ((atomic? te) #t)
          ((variable? te) (not (eq? var te)))
          ((composite? te)
           (fold (lambda (b1 b2) (and b1 b2))
                 #t
                 (flatten
                  (map (lambda (te-element) (non-circular? var te-element))
                       te)))) 
          (else (error 'non-circular? "Bad syntax in Type expression type: ~s" te)))
    ))

; Getters:

;Signature: get-variables(sub)
;Type: [LIST -> LIST(Symbol)] 
;Purpose: get variables of substitution 
;Tests: (get-variables (make-sub '(x y z) 
;                        (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number))) ) ==> 
;                                                                                                     (x y z)
(define get-variables
  (lambda (sub) (car sub)))

;Signature: get-tes(sub)
;Type: [LIST -> LIST(LIST union Symbol)] 
;Purpose: getting type expressions of substitution
;Tests: (get-tes 
;           (make-sub '(x y z) (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number))))  
;                                             ==>  (Number Boolean (-> (* Number) Number))
(define get-tes
  (lambda (sub) (cadr sub)))

;Signature: get-expression-of-variable(sub,var)
;Type: [LIST*Symbol -> LIST union Symbol] 
;Purpose: getting expression of cirtain variable
;Tests: (get-expression-of-variable 
;           (make-sub '(x y z) 
;                     (list 'Number 'Boolean 
;                         (make-proc-te (make-tuple-te (list 'Number)) 'Number)))
;           'y ) ==> Boolean 
;Pre-condition: sub is a 2 element list of equal length lists of variables and tes.
(define get-expression-of-variable
  (lambda (sub var)
    (let ((variables (get-variables sub))
          (tes (get-tes sub)))
      (cond ((null? variables) 'Empty)
            ((eq? var (car variables)) (car tes))
            (else (get-expression-of-variable
                   (make-sub (cdr variables) (cdr tes)) var))))
    ))
          

;;;;;;;;;;;;;;;
;identifiers:

;Signature: sub?(sub)
;Type: [T -> Boolean] 
;Purpose: is substitution
;Tests: (sub? (make-sub '(x y z) 
;                         (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number)))) ==> #t
;         (sub? (make-sub (list) (list))) ==> #t
;         (sub? '()) ==> #f
(define sub?
  (lambda (sub) 
    (and (not (null? sub)) 
         (list? (car sub))
         (not (null? (cdr sub)))
         (null? (cddr sub))
         (let ((variables (get-variables sub))
               (tes (get-tes sub)))
           (and (eq? (length variables) (length tes))
                (equal? (map type-expr? tes)
                        (make-list (length tes) #t)))))
    ))


;Signature: empty-sub?(sub)
;Type: [LIST -> Boolean] 
;Purpose: is the substitution empty
;Tests: (empty-sub? (make-sub '(x y z) 
;                       (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number)))) ==> #f
;         (empty-sub? (make-sub (list) (list))) ==> #t
;Pre-condition: (sub? sub)
(define empty-sub?
  (lambda (sub)
    (and (sub? sub)
         (null? (get-variables sub))
         (null? (get-tes sub)))
    ))
                


; Signature: substitution-application(sub,te)
;Type: Client view: [Substitution*Type-expression -> Type-expression]
;      implementation view: [LIST*(LIST union Symbol) -> LIST union Symbol]
;Purpose: applies the substitution to a given type expression
;Tests: (substitution-application 
;            (make-sub '(T1 T2 T3) (list 'Number (make-tuple-te (list 'T4 'Number)) 'Boolean))
;            (make-tuple-te (list 'T2 (make-proc-te (make-tuple-te (list 'Number 'T1)) 'T1))) ) ==>
;                                               (* (* T4 Number) (-> (* Number Number) Number))
(define substitution-application
  (lambda (sub te)
    (if (null? te) 
        null 
        (if (empty-sub? sub) 
            (display 'Error-With-Input)         ;empty_substitution or not sub at all
         (append (list (swap-type sub (car te))) 
                 (substitution-application sub (cdr te))))    
    )))

;Signature: swap-type(sub var)
;Type: 
;Purpose: check what is the type of te, and swap it if can
;Tests:  

(define swap-type
  (lambda (sub var)
    (if (list? var) 
        (substitution-application sub var)
    (if (or (eq? var '*) (eq? var '->)) var
      (if (variable? var) 
        (if (eq? (get-expression-of-variable sub var) 'Empty)
            var
             (get-expression-of-variable sub var)
         )
        var
      ) 
)
    )))

;Signature: substitution-combination(sub1,sub2)
;Type: Client view: [Substitution*Substitution -> Substitution]
;      implementation view: [LIST*LIST -> LIST]
;Purpose: combines to substitution to a single substitution
;Tests: (substitution-combination 
;  (make-sub '(T1 T2 T3) 
;            (list 'S1 
;                  (make-tuple-te (list 'S2 'Number)) 
;                  'Boolean))
;  (make-sub '(S1 S2)
;    (list 
;      (make-tuple-te 
;         (list 'T21
;               (make-proc-te 
;                  (make-tuple-te (list 'Number 'T11)) 
;                  'T11)))
;      'T31))) ==>
;  '((S2 S1 T1 T2 T3) 
;(T31 (* T21 (-> (* Number T11) T11)) (* T21 (-> (* Number T11) T11)) (* T31 Number) Boolean));
(define substitution-combination
  (lambda (sub1 sub2)
        (make-sub (get-vars sub2 (make-sub (get-variables sub1) (substitution-application sub2 (get-tes sub1))))
                  (get-types sub2 (make-sub (get-variables sub1) (substitution-application sub2 (get-tes sub1)))))))

;Signature: get-vars(sub2,sub1)
;Type: Client view: [Substitution*Substitution -> List]
;Purpose: Return a combine variables list upon rules from sub1 and sub2
;Tests: (get-vars 
;  (make-sub '(T1 T2 T3) 
;            (list 'Number 
;                  'Numberico 
;                  'Boolean))
; (make-sub '(T4 T100 T6)
;   (list 
;        'T41
;        'T51
;        'T61))) => '(T6 T100 T4 T1 T2 T3)   
(define get-vars
  (lambda (sub2 sub1)
    (if (empty-sub? sub2) (get-variables sub1)
            (if (eq? (memq (car (get-variables sub2)) (get-variables sub1)) #f)                           
                        (get-vars (make-sub (cdr (get-variables sub2)) (cdr (get-tes sub2)))
                                  (make-sub (cons (car (get-variables sub2)) (get-variables sub1))
                                            (cons (car (get-tes sub2)) (get-tes sub1))))
                        (get-vars (make-sub (cdr (get-variables sub2)) (cdr (get-tes sub2)))
                                    sub1)
             ))))

;Signature: get-types(sub2,sub1)
;Type: Client view: [Substitution*Substitution -> List]
;Purpose: Return a combine types expression list upon rules from sub1 and sub2
;Tests: (get-vars 
;  (make-sub '(T1 T2 T3) 
;            (list 'Number 
;                  'Numberico 
;                  'Boolean))
; (make-sub '(T4 T100 T6)
;   (list 
;        'T41
;        'T51
;        'T61))) => '(T61 T51 T41 Number Numberico Boolean)
(define get-types
  (lambda (sub2 sub1)
    (if (empty-sub? sub2) (get-tes sub1)
            (if (eq? (memq (car (get-variables sub2)) (get-variables sub1)) #f)                           
                        (get-types (make-sub (cdr (get-variables sub2)) (cdr (get-tes sub2)))
                                  (make-sub (cons (car (get-variables sub2)) (get-variables sub1))
                                            (cons (car (get-tes sub2)) (get-tes sub1))))
                        (get-types (make-sub (cdr (get-variables sub2)) (cdr (get-tes sub2)))
                                    sub1)
             ))))                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auxiliaries

;Signature: flatten(tree)
;Type: Client view: [Substitution*Substitution -> Substitution]
;      implementation view: [LIST*LIST -> LIST]
;Purpose: Flatten a heterogeneous list
;Tests: (flatten '((1) 2) => '(1 2)
(define flatten
    (lambda (tree)
      (if (not (list? tree)) 
          (list tree)
          (fold append (list) (map flatten tree)))
     ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;