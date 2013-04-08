#lang racket
; Signature: ln2-rec(n)
; Type: [T -> Number]
; Purpose: Calculates ln2 n-th item upon the sigma function recursivly
; Pre-conditions: n > 0,an integer
; Post-conditions: result
; Tests: (ln2-rec 2) ==> 0.5
;        (ln2-rec 3) ==> 0.8333333333333333
;        (ln2-rec 1000) ==> 0.6926474305598223
(define ln2-rec 
(lambda (n)
  (if (= n 0) 0 
      (+ (if (even? n) (/ -1. n) (/ 1. n))  (ln2-rec (- n 1) ) ))))

; Signature: third-iter( n acc )
; Type: [T x Number -> Number]
; Purpose: Calculates ln2 n-th item upon the sigma function iterativly
; Pre-conditions: n > 0,an integer
; Post-conditions: result
; Tests: (third-iter 2 0) ==> 0.25
;        (third-iter 4 0) ==> 0.3125
;        (third-iter 20 0) ==> 0.33333301544189453

(define third-iter
 (lambda (n acc)
 (cond ( (= n 0) (display acc) )
 (else (third-iter (- n 1) (+ acc (if (even? n) (/ -1. (expt 2 n)) (/ 1. (expt 2 n))))))))) 

;Signature: sum-alt( term a next b )
;Purpose: to compute the sum of terms, defined by <term>
;    in predefined gaps, defined by <next>, in the interval [a,b] as an alternative series.
;Type: [[Number -> Number]*Number*[Number -> Number]*Number -> Number]
;Post-conditions: result = (term a) + (term (next a)) + ... (term n),
;                     where n = (next (next ...(next a))) =< b,
;                                                (next n) > b.
;Example: (sum identity 1 add1 3) should produce 2,
;             where ’identity’ is (lambda (x) x)

(define sum-alt
   (lambda (term a next b)
    (if (> a b)
        0
     (if (even? a) 
        (+ (* -1. (term a)) (sum-alt term (next a) next b))
        (+ (term a) (sum-alt term (next a) next b))
        ))))

; Signature: term-ln2(n)
; Type: [T -> Number]
; Purpose: Calculates ln2 n-th (and only the n-th plce) item upon the sigma function
; Pre-conditions: n > 0,an integer
; Post-conditions: result
; Tests: (term-ln2 2) ==> -0.5
;        (term-ln2 3) ==> 0.3333333333333333

(define term-ln2 
(lambda (n)
   (if (= n 0) 0
       (/ 1. n))))

; Signature: next-ln2(n)
; Type: [T -> Number]
; Purpose: Calculates ln2 n+1-th (and only the n+1-th plce) item upon the sigma function
; Pre-conditions: n > 0,an integer
; Post-conditions: result
; Tests: (next-ln 2) ==> 0.3333333333333333

(define next-ln2 
(lambda (n)
  (+ n 1)))

; Signature: term-third(n)
; Type: [T -> Number]
; Purpose: Calculates 1/3 conveges series at the n-th (and only the n-th plce) item upon the sigma function
; Pre-conditions: n > 0,an integer
; Post-conditions: result
; Tests: (term-third 1) ==> 0.5

(define term-third
 (lambda (n)
   (if (= n 0) 0
 (/ 1. (expt 2 n))))) 

; Signature: next-third(n)
; Type: [T -> Number]
; Purpose: Calculates 1/3 conveges series at the n+1-th (and only the n+1-th plce) item upon the sigma function
; Pre-conditions: n > 0,an integer
; Post-conditions: result
; Tests: (next-third 3) ==> -0.0625

(define next-third
 (lambda (n)
  (+ n 1)))
   ; if (= n 0) (void) 
      ;(if (even? n) (/ 1. (expt 2 (+ n 1))) (/ -1. (expt 2 (+ n 1)))))))


; Signature: con-func(f a flag)
; Type: [[Number->Number]*Number*Number -> Number]
; Purpose: procedure (con-func f a flag) that given a one-argument function f, a number a and a number flag returns the function g:
;- g (x)= f (x + a) if flag =0
;- g (x)= f (x - a) if flag =1
;- g (x)= f (x * a) if flag =2
;- g (x)= f (x / a) if flag =3
;- and g (x)= f (x) otherwise
; Tests: (ln2-rec 2) ==> 0.5
;        (ln2-rec 3) ==> 0.8333333333333333
;        (ln2-rec 1000) ==> 0.6926474305598223
(define con-func
  (lambda (f a flag)
    
    (cond 
      ((= flag 0) (lambda(x) (f (+ x a))))
      ((= flag 1) (lambda(x) (f (- x a))))
      ((= flag 2) (lambda(x) (f (* x a))))
      ((= flag 3) (lambda(x) (f (/ x a))))
      (else (lambda(x) (f x))))))