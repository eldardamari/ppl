#lang racket
(require "substitution-core.rkt" "asp.rkt" "ge-adt.rkt")

(run-tests
 (derive-eval '(define foo
                 (lambda ()
                   1)))
 (derive-eval '(define goo
                 (lambda (x)
                   x)))
 
 (test (parameter-less?
        (lookup-variable-value 
         'foo)) => #t)
 
 (test (parameter-less?
        (lookup-variable-value 
         'goo)) => #f) 
 )
