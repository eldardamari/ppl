#lang racket

(provide (all-defined-out))


;Signature: enumerate-tree$(tree,cont)
;Purpose: List all leaves of a number tree
;Type: [LIST union T -> LIST(Number)]
;Tests: (enumerate-tree (list 1 (list 2 (list 3 4)) 5)) ==> (1 2 3 4 5)
;Post-condition: result = flatten(tree)
(define enumerate-tree$
  (lambda (tree cont)
    (cond ((null? tree) (cont tree))
          ((not (list? tree)) (cont (list tree)))
          (else (enumerate-tree$ (car tree)
                                 (lambda (car-tree-res)
                                   (enumerate-tree$ (cdr tree)
                                                    (lambda (cdr-tree-res)
                                                      (cont (append car-tree-res cdr-tree-res))))))))))

;Signature: replace->and+(expr)
;Purpose: Find the left most * and replace it with + and find the
;              right most -> and replace it with => . If none return expr
;Type: [LIST union Symbol -> LIST union Symbol]
;Tests: (replace->and+ (make-proc-te (make-tuple-te (list 'Number)) (make-proc-te (make-tuple-te (list 'Number)) 'Number)))
;                              => '(-> (+ Number) (=> (* Number) Number)))
(define replace->and+
  (lambda(expr)
    (cond ((not (list? expr))
           (cond ((eq? expr '*) '+)
                 ((eq? expr '->) '=>)
                 (else expr)))
          (else
           (let ((ans+ (replace->and+$ expr '* '+ (lambda(x) x) (lambda() #f))))
             (if (eq? ans+ #f)
                 (reverse (replace->and+$ (reverse expr) '-> '=> (lambda(x) x) (lambda() #f)))
                 (let ((ans-> (replace->and+$ (reverse ans+) '-> '=> (lambda(x) x) (lambda() #f))))
                   (if (eq? ans-> #f)
                       ans+
                       (reverse ans->)))
                 )
             )
           )
          )
    ))
                                  
;Signature: replace->and+$(expr,old,new,succ-cont,fail-cont)
;Purpose: Find the left most leaf whoose value is 'old' and replace it by 'new'. If none, return #f.
;Type: [(LIST union Symbol)*T1*T2*T3*T4 -> LIST union Boolean]
;Test: (replace->and+$ (make-proc-te (make-tuple-te (list 'Number)) 'Number) '* '+ (lambda(x) x) (lambda() #f))
;                        => '(-> (+ Number) Number)
(define replace->and+$
  (lambda (expr old new succ-cont fail-cont)
    (cond ((null? (fail-cont)))
          ((not (pair? expr))
           (if (eq? expr old)
               (succ-cont new)
               (fail-cont)))
           (else
            (replace->and+$ (car expr)
                            old
                            new
                            (lambda (car-res)
                              (succ-cont (cons car-res (cdr expr))))
                            (lambda ()
                              (replace->and+$ (cdr expr)
                                              old
                                              new
                                              (lambda (cdr-res)
                                                (succ-cont (cons (car expr) cdr-res)))
                                              fail-cont)))) )) )