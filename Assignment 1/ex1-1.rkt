; Signature: deep-sum-digit(n)
; Type: [T -> Number] 
; Purpose: Calculates the sum of digits of the sum of digits up to a number smaller than 10
; Pre-conditions: n > 0,an integer
; Post-conditions: result
; Tests: (deep-sum-digit 4526) => 8
;      : (deep-sum-digit 1342) => 1
;      : (deep-sum-digit 673) => 7
(define deep-sum-digit 
(lambda (n)
(if (< n 10)
n
(deep-sum-digit(+ (modulo n 10) (deep-sum-digit (floor (/ n 10))))))))
 
; Signature: calc-1-e(n) 
; Type: [Number -> Number]
; Purpose: Approximates 1/e as the sum of n first elements from n_i=(-1)^i/i!
; Pre-conditions: n>=0, integer 
; Tests: 
(define calc-1-e
  (lambda (n)
    (sum-sigma n 0)))
 
; Signature: sum-sigma(num,k)
; Type: [Number*Number -> Number]
; Purpose: Approximates 1/e as the sum of n first elements from n_i=(-1)^i/i!
; Pre-conditions: n>=0, integer 
; Tests: 
(define sum-sigma
  (lambda (num k)
    (if (= num k) (sigma k) (+ (sigma k) (sum-sigma num (+ k 1))))))

; Signature: sigma(k)
; Type: [Number -> Number]
; Purpose: Iterating the Sigma and return the sum.
; Pre-conditions: n>=0, integer 
; Tests: 
(define sigma
  (lambda (k)
    (if (even? k) (/ 1. (fact-iter 1 1 k)) (/ -1. (fact-iter 1 1 k)))))

; Signature: fact-iter(product,counter,max-count)
; Purpose: to compute the factorial of a number ’max-count’.
; This procedure follows the rule:
;       counter = 1;  product = 1;
;       repeat the simultaneous transformations:
;       product <-- counter * product,    counter <-- counter + 1.
;       stop when counter > n.
; Type: [Number*Number*Number -> Number]
; Pre-conditions:
;        product, counter, max-count > 0
;       product * counter * (counter + 1) * ... * max-count = max-count!
; Post-conditions: result = max-count!
; Example: (fact-iter 2 3 4) should produce 24
; Tests: (fact-iter 1 1 1)  ==> 1
;        (fact-iter 1 1 4) ==> 24
(define fact-iter
   (lambda (product counter max-count)
      (if (> counter max-count)
          product
          (fact-iter (* counter product)
                     (+ counter 1)
                     max-count))))