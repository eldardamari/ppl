#lang racket

(require "utils.rkt")

; Load student's solution file
(include "ex1-1.rkt")

; Tests for question 3
(define (q3-tests) 
  (run-tests
   (test (deep-sum-digit 4526) => 8)
   (test (deep-sum-digit 1342 ) => 1)
   (test (deep-sum-digit 1111111111) => 1)
   (test (deep-sum-digit 123456789) => 9)))



; Tests for question 4
(define delta 0.000000001)
(define (q4-tests) 
  (run-tests
   (test (< (abs (- (calc-1-e 1) 0)) delta) => #t)  
   (test (< (abs (- (calc-1-e 2) (/ 1 2))) delta) => #t)
   (test (< (abs (- (calc-1-e 10) (/ 16481 44800))) delta) => #t) 
   (test (< (abs (- (calc-1-e 5) (/ 11 30))) delta) => #t))) 


; Run the tests
(q3-tests)
(q4-tests)