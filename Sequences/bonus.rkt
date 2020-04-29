;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ****************************************
;;   Julian Dominguez-Schatz (20792318)
;;   CS 135, Fall 2018
;;   Assignment 9, Bonus Question
;; ****************************************
;;

;(require racket/base)

;; ==== BONUS QUESTION ====================


(define tests '((() ())
                ((2 4) (6 8 10 12))
                ((0 1) (2 3 4 5))
                ((1 11 33) (67 113))
                ((2 4 8 16) (32 64))
                ((1) (1 1 1 1 1 1))
                ((1 2) (3 4 5 6 7 8))
                ((1 1 2 3) (5 8 13 21 34 55))
                ((1 3 9 27) (81 243 729 2187 6561 19683))
                ((1 -1 1 -1 1) (-1 1 -1 1 -1 1 -1 1))
                ((1 -2 4 -8) (16 -32 64 -128 256 -512 1024))
                ((1 0 1 4 17) (72 305 1292))
                ((0 0 1 0 1 2) (0 1 2 3))
                ((1 2 2 4 8) (32 256 8192))
                ((3 1 6 12 144) (3456 995328))
                ((1 1 2 6 24) (120 720))
                ((1 4 32 384 6144) (122880 2949120))
                ((1 3 6 10) (15 21 28 36 45))
                ((1 4 10 20 35) (56 84 120))
                ((2) (2 2 2 2 2 2 2 2 2 2 2 2))
                ((1 4 10 20 1) (4 10 20 1 4 10 20))
                ((1 4 10 1 4) (10 1 4 10 1 4 10))
                ((1 4 1 4) (1 4 1 4 1 4))
                ((1 11 111 1111) (11111 111111 1111111 11111111))
                ((12 1212 121212 12121212) (1212121212 121212121212))
                ((12 34 56 78 910) (1112 1314 1516 1718))
                ((11 23 58 1321 3455) (89144))
                ((111 1111111 11111111111 111111111111111)
                 (1111111111111111111 11111111111111111111111))
                ((1 11 21 1211 111221)
                 (312211 13112221 1113213211 31131211131221))
                ((67 1617 11161117 31163117 132116132117)
                 (11131221161113122117 311311222116311311222117))
                ((2 3 5 7 11) (13 17 19 23))
                ((2 -3 -5 7 11) (-13 -17 19 23))))

(define test-01 '(2 4 6 8 10 12))
(define test-02 '(0 1 2 3 4 5))
(define test-03 '(1 11 33 67 113))
(define test-04 '(2 4 8 16 32 64))
(define test-05 '(1))
(define test-06 '(1 2))
(define test-07 '(1 1 2 3 5 8 13 21 34 55))
(define test-08 '(1 3 9 27 81 243 729 2187 6561 19683))
(define test-09 '(1 -2 4 -8 16 -32 64 -128 256 -512 1024))
(define test-10 '(1 0 1 4 17 72 305))
(define test-11 '(0 0 1 0 1 2 0 1 2 3))
(define test-12 '(1 2 2 4 8 32 256 8192))
(define test-13 '(3 1 6 12 144 3456 995328))
(define test-14 '(1 1 2 6 24 120 720))
(define test-15 '(1 4 32 384 6144 122880 2949120))
(define test-16 '(1 3 6 10 15 21 28 36 45))
(define test-17 '(1 4 10 20 35 56 84 120))
(define test-18 '(2 2 2 2 2 2 2 2 2 2))
(define test-19 '(1 4 10 20 1 4 10 20))


;; ==== General Helper Functions ==========


;; (pad-list v n lox) produces a list identical to lox, but with v
;;   repeatedly attached to the back such that (length lox) >= n.
;; pad-list: X Nat (listof Y) -> (listof (anyof X Y))
;; Examples:
(check-expect (pad-list 'empty 2 empty) '(empty empty))
(check-expect (pad-list 1 4 '(0 0)) '(0 0 1 1))
(check-expect (pad-list 1 3 '(0 1 2 3 4)) '(0 1 2 3 4))

(define (pad-list v n lox)
  (cond
    [(<= (- n (length lox)) 0) lox]
    [else (append (pad-list v (sub1 n) lox) (list v))]))


;; (list-head n lox) produces a list of the first n elements of loa.
;; list-head: Nat (listof X) -> (listof X)
;; Examples:
(check-expect (list-head 4 '(a b c d)) '(a b c d))
(check-expect (list-head 4 '(a b c d e)) '(a b c d))
(check-expect (list-head 0 '(0 1 e e)) empty)
(check-expect (list-head 4 empty) empty)
(check-expect (list-head 0 empty) empty)

(define (list-head n lox)
  (foldr (λ (index x result)
           (cond
             [(>= index n) result]
             [else (cons x result)]))
         empty (build-list (length lox) identity) lox))


;; (list-tail n lox) produces a list of the last n elements of loa.
;; list-tail: Nat (listof X) -> (listof X)
;; Examples:
(check-expect (list-tail 4 '(a b c d)) '(a b c d))
(check-expect (list-tail 4 '(a b c d e)) '(b c d e))
(check-expect (list-tail 0 '(0 1 e e)) empty)
(check-expect (list-tail 4 empty) empty)
(check-expect (list-tail 0 empty) empty)

(define (list-tail n lox)
  (local [(define len (length lox))]
    (foldr (λ (index x result)
             (cond
               [(< index (- len n)) result]
               [else (cons x result)]))
           empty (build-list len identity) lox)))


;; (remove-last lox) produces a list identical to lox but with the
;;   last element removed.
;; remove-last: (listof X) -> (listof X)
;; Examples:
(check-expect (remove-last '(0 1 2 3 4 5)) '(0 1 2 3 4))
(check-expect (remove-last empty) empty)

(define (remove-last lox)
  (cond
    [(empty? lox) empty]
    [(empty? (rest lox)) empty]
    [else (cons (first lox) (remove-last (rest lox)))]))


;; (cons-all el lol) produces a list identical to lol but with el
;;   added to the front of all the sublists.
;; cons-all: X (listof (listof X)) -> (listof (listof X))
;; Examples:
(check-expect (cons-all 2 '(() ())) '((2) (2)))
(check-expect (cons-all 2 empty) empty)

(define (cons-all el lol)
  (cond
    [(empty? lol) empty]
    [else (cons (cons el (first lol)) (cons-all el (rest lol)))]))


;; (cons-end-in el n lol) produces a list identical to lol but with el
;;   added to the front of the nth child of lol.
;; cons-end-in: X Nat (listof (listof X)) -> (listof (listof X))
;; Examples:
(check-expect (cons-end-in 2 2 '((0) () (0 1)))
              '((0) () (0 1 2)))
(check-expect (cons-end-in 2 0 empty) '((2)))
(check-expect (cons-end-in 2 1 empty) '(() (2)))

(define (cons-end-in el n lol)
  (cond
    [(and (zero? n) (empty? lol))
     (list (list el))]
    [(zero? n) (cons (append (first lol) (list el)) (rest lol))]
    [(empty? lol) (cons empty (cons-end-in el (sub1 n) empty))]
    [else (cons (first lol) (cons-end-in el (sub1 n) (rest lol)))]))


;; (cons-last el lst) produces a list identical to lst but with el
;;   added on at the end.
;; cons-last: X (listof X) -> (listof X)
;; Examples:
(check-expect (cons-last 4 '(0 1 2 3)) '(0 1 2 3 4))
(check-expect (cons-last 4 empty) '(4))

(define (cons-last el lst)
  (cond
    [(empty? lst) (list el)]
    [else (cons (first lst) (cons-last el (rest lst)))]))


;; (list-part n lox) produces a list of n lists of the elements of
;;   lox, alternated between the n lists. See the examples.
;; list-part: Nat (listof X) -> (listof (listof X))
;; requires: n > 0
;; Examples:
(check-expect (list-part 2 '(0 1 2 3 4 5)) '((0 2 4) (1 3 5)))
(check-expect (list-part 3 '(0 0 0 1 1 1 2)) '((0 1 2) (0 1) (0 1)))
(check-expect (list-part 3 empty) empty)

(define (list-part n lox)
  (local [;; (list-part/acc m n lox result) produces a list of n lists
          ;;   of the elements of lox, alternated between the n lists.
          ;; list-part/acc: Nat Nat (listof X) (listof (listof X))
          ;;                  -> (listof (listof X))
          ;; requires: m > 0
          (define (list-part/acc m n lox result)
            (cond
              [(empty? lox) result]
              [else (list-part/acc m (add1 n) (rest lox)
                                   (cons-end-in (first lox)
                                                (remainder n m)
                                                result))]))]
    (list-part/acc n 0 lox empty)))


;; (list-permute n lox) produces a list of all the permutations
;;   of n-length lists made of elements of lox.
;; list-permute: Nat (listof X) -> (listof (listof X))
;; requires: n > 0
;;           lox contains no repeated elements
;; Examples:
(check-expect (list-permute 2 '(0 1)) '((0 0) (0 1) (1 0) (1 1)))
(check-expect (list-permute 1 '(0 1)) '((0) (1)))
(check-expect (list-permute 1 empty) empty)

(define (list-permute n lox)
  (cond
    [(zero? n) '(())]
    [else (foldr (λ (f r-t)
                   (append (cons-all f (list-permute (sub1 n) lox)) r-t))
                 empty lox)]))


;; the error message produced whenever (last empty) occurs
(define last-error-empty "last: expects a non-empty list; given: empty")


;; (last lst) produces the last element of lst.
;; last: (listof X) -> X
;; Examples:
(check-expect (last '(0 1 2 3 4)) 4)
(check-expect (last '(0)) 0)
(check-error (last empty))

(define (last lst)
  (cond
    [(empty? lst) (error last-error-empty)]
    [(empty? (rest lst)) (first lst)]
    [else (last (rest lst))]))

;; (index-of el lst) produces the index of el in lst, or false if
;;   there is no such index.
;; index-of: X (listof X) -> (anyof Nat false)
;; Examples:
(check-expect (index-of 4 empty) false)
(check-expect (index-of 4 '(0 1 2 3 5 4)) 5)

(define (index-of el lst)
  (local [;; (index-of/counter el lst counter) produces the index
          ;;   of el in lst, or false if there is no such index.
          ;; index-of/counter: X (listof X) Nat -> (anyof Nat false)
          (define (index-of/counter el lst counter)
            (cond
              [(empty? lst) false]
              [(equal? el (first lst)) counter]
              [else (index-of/counter el (rest lst) (add1 counter))]))]
    (index-of/counter el lst 0)))


;; (list-abs lon) produces a list identical to lon but with
;;   all numbers non-negative.
;; negate-list: (listof Num) -> (listof Num)
;; Examples:
(check-expect (list-abs '(0 1 -4 -5 7)) '(0 1 4 5 7))
(check-expect (list-abs empty) empty)

(define (list-abs lon)
  (map abs lon))


;; (group-dup lst) produces a list of lists, where each element is
;;   a pair. The first of the pair is an element of lst, and the second
;;   is how many times it appeared consecutively.
;; group-dup: (listof X) -> (listof (list Nat X))
;; Examples:
(check-expect (group-dup '(0 1 2 3 4)) '((1 0) (1 1) (1 2) (1 3) (1 4)))
(check-expect (group-dup '(0 0 0 1 1)) '((3 0) (2 1)))
(check-expect (group-dup empty) empty)

(define (group-dup lst)
  (foldr (λ (el result)
           (cond
             [(and (cons? result) (equal? el (second (first result))))
              (cons (list (add1 (first (first result)))
                          (second (first result)))
                    (rest result))]
             [else (cons (list 1 el) result)]))
         empty
         lst))


;; A (nested X) is a (listof (anyof (nested X) X))

;; (flatten lst) produces a list identical to lst but with all elements
;;   of any sublists of lst combined into one big list.
;; flatten: (nested X) -> (listof X)
;; Examples:
(check-expect (flatten '(0 1 2 3)) '(0 1 2 3))
(check-expect (flatten '(0 (1 (2 (3))))) '(0 1 2 3))
(check-expect (flatten '((0) (1) (2) (3))) '(0 1 2 3))
(check-expect (flatten empty) empty)
(check-expect (flatten '(())) empty)

(define (flatten lst)
  (foldr (λ (el r-t)
           (cond
             [(list? el)
              (append (flatten el) r-t)]
             [else (cons el r-t)]))
         empty
         lst))


;; (digits->nat lst) produces the number represented by the digits in lst.
;; digits->nat: (listof Nat) -> Nat
;; Examples:
(check-expect (digits->nat '(0 1 2 3 4)) 1234)
(check-expect (digits->nat '(0 10 2 3 4)) 10234)
(check-expect (digits->nat empty) 0)

(define (digits->nat lst)
  (foldl (λ (n acc)
           (+ n (* (expt number-base (digit-count n)) acc)))
         0
         lst))


;; (safe-apply f d n args) produces the result of (f a0 a1 a2 ...)
;;   where a0, a1, a2 ... are the elements of args, and args has been
;;   padded with d or truncated so that it has length exactly n.
;; safe-apply: (X ... -> Y) X Nat (listof X) -> Y
;; requires: f takes exactly n parameters
;; Examples:
(check-expect (safe-apply + 0 4 '(1 2 3 4)) 10)
(check-expect (safe-apply + 0 4 empty) 0)
(check-expect (safe-apply > 0 2 '(2 4)) false)
(check-expect (safe-apply > 0 2 '(2)) true)
(check-expect (safe-apply > 0 2 empty) false)

(define (safe-apply f d n args)
  (apply f (list-head n (pad-list d n args))))


;; (zero-divide dividend divisor) produces the result of dividing
;;   dividend by divisor, or 0 if divisor is 0.
;; zero-divide: Num Num -> Num
;; Examples:
(check-expect (zero-divide 10 5) 2)
(check-expect (zero-divide 10 0) 0)

(define (zero-divide dividend divisor)
  (cond
    [(zero? divisor) 0]
    [else (/ dividend divisor)]))


;; (list-divide lon1 lon2) produces a list of the numbers in lon1
;;   safely divided by the numbers in lon2.
;; list-divide: (listof Num) (listof Num) -> (listof Num)
;; requires: lon1 and lon2 have the same length
;; Examples:
(check-expect (list-divide '(1) '(2)) '(1/2))
(check-expect (list-divide '(0 1 3) '(0 0 2)) '(0 0 3/2))
(check-expect (list-divide empty empty) empty)

(define (list-divide lon1 lon2)
  (map zero-divide lon1 lon2))


;; (! n) produces the value of n!, i.e. n*(n-1)*(n-2)...*2*1,
;;   or 1 for n=0.
;; !: Nat -> Nat
;; Examples:
(check-expect (! 0) 1)
(check-expect (! 4) 24)
(check-expect (! 10) 3628800)

(define (! n)
  (cond
    [(= n 0) 1]
    [else (* n (! (sub1 n)))]))


;; (falling-! n m) produces the value of n_m, i.e. n!/(n-m)!.
;; falling-!: Nat Nat -> Nat
;; requires: m <= n
;; Examples:
(check-expect (falling-! 0 0) 1)
(check-expect (falling-! 4 2) 12)
(check-expect (falling-! 10 6) 151200)

(define (falling-! n m)
  (/ (! n) (! (- n m))))


;; (nCr n r) produces the value of n choose r.
;; nCr: Nat Nat -> Nat
;; requires: r <= n
;; Examples:
(check-expect (nCr 0 0) 1)
(check-expect (nCr 1 0) 1)
(check-expect (nCr 1 1) 1)
(check-expect (nCr 10 4) 210)

(define (nCr n r)
  (/ (falling-! n r) (! r)))


;; used to convert a number into a list of digits
(define number-base 10)

;; (nat->digits n) produces a list of all the digits in i.
;; nat->digits: Nat -> (listof (anyof 0 1 2 3 4 5 6 7 8 9))
;; Examples:
(check-expect (nat->digits 0) '(0))
(check-expect (nat->digits 15) '(1 5))

(define (nat->digits n)
  (cond
    [(< n number-base) (list n)]
    [else (cons-last (remainder n number-base)
                     (nat->digits (quotient n number-base)))]))


;; the first prime number
(define first-prime 2)


;; (prime? n primes) determines whether n is a prime number, using
;;   the list of known primes to speed up computation.
;; prime?: Nat (listof Nat) -> Bool
;; requires: the primes in primes are strictly decreasing
;;           the list of primes does not contain 2
;;           the list of primes is complete, up to the largest element
;; Examples:
(check-expect (prime? 0 empty) false)
(check-expect (prime? 1 empty) false)
(check-expect (prime? 2 empty) true)
(check-expect (prime? 4 empty) false)
(check-expect (prime? 199 '(11 7 5 3)) true)
(check-expect (prime? 39 empty) false)
(check-expect (prime? 45 '(3)) false)

(define (prime? n primes)
  (local [(define sqrt-n (sqrt n))
          (define (check-prime divisor)
            (cond
              [(> divisor sqrt-n) true]
              [(zero? (remainder n divisor)) false]
              [else (check-prime (+ 2 divisor))]))]
    (cond
      [(< n first-prime) false]
      [(= n first-prime) true]
      [(even? n) false]
      [(ormap (λ (prime) (zero? (remainder n prime))) primes) false]
      [else (check-prime (cond
                           [(empty? primes) 3]
                           [else (first primes)]))])))

;; (prime-list n) produces a list of the first n primes.
;; prime-list: Nat -> (listof Nat)
;; Examples:
(check-expect (prime-list 0) empty)
(check-expect (prime-list 1) '(2))
(check-expect (prime-list 10) '(29 23 19 17 13 11 7 5 3 2))

(define (prime-list n)
  (local [(define (prime-list/acc current lst count)
            (cond
              [(>= count n) lst]
              [else (cond
                      [(prime? current lst)
                       (prime-list/acc (add1 current)
                                       (cons current lst)
                                       (add1 count))]
                      [else (prime-list/acc (add1 current)
                                            lst count)])]))]
    (prime-list/acc first-prime empty 0)))

;; the reciprocal of the logarithm of 10; used to compute base 10 log
(define base-10-log-factor (/ 1 (log 10)))

;; (digit-count x) produces the number of digits in x.
;; digit-count: Nat -> Nat
;; Examples:
(check-expect (digit-count 0) 1)
(check-expect (digit-count 15) 2)
(check-expect (digit-count 15782938) 8)

(define (digit-count x)
  (cond
    [(<= x 0) 1]
    [(zero? (remainder x 10)) (digit-count (add1 x))]
    [else (add1 (inexact->exact (floor (* base-10-log-factor (log x)))))]))


;; ==== Pattern-Related Helper Functions ==


;; A Sample is a (listof Int)

;; A Solution is a (Nat -> Int)

;; A PartialPattern is a (list (Int ... -> Solution) Nat)
;; requires: the function has the same number of parameters as the Nat

;; A FullPattern is a (Sample -> (anyof Solution empty))

;; A Pattern is (anyof PartialPattern FullPattern)


;; (try-pattern patterns pattern-to-try lon) tries solving the number
;;   sequence lon with pattern-to-try and the patterns in patterns. It
;;   produces the solution if it works and empty if it doesn't.
;; try-pattern: (listof Pattern) Pattern Sample -> (anyof Solution empty)
(define (try-pattern patterns pattern-to-try lon)
  (local [;; try-pattern/opt: Bool -> (anyof Solution empty)]))
          (define (try-pattern/opt absolute)
            (local [(define lst (cond
                                  [absolute (list-abs lon)]
                                  [else lon]))
                    (define guess (cond
                                    [(list? pattern-to-try)
                                     (safe-apply (first pattern-to-try) 0
                                                 (second pattern-to-try) lst)]
                                    [else (pattern-to-try patterns lst)]))]
              (cond
                [(empty? guess) empty]
                [(solution? guess lst)
                 (cond
                   [absolute (local [(define oscillator
                                       (try-all patterns
                                                (list-divide lon lst)))]
                               (λ (n)
                                 (* (oscillator n) (guess n))))]
                   [else guess])]
                [(or absolute (andmap (λ (term) (= 1 (abs term))) lst)) empty]
                [else (try-pattern/opt true)])))]
    (try-pattern/opt false)))


;; (solution? f lon) checks that f generates the values in
;;   lon, where f takes a position in the list as an argument,
;;   starting at 0.
;; solution?: Solution Sample -> Bool
;; Examples:
(check-expect (solution? identity '(0 1 2 3 4 5 6)) true)
(check-expect (solution? add1 '(1 2 3 4 5 6)) true)
(check-expect (solution? sub1 '(1 2 3 4 5 6)) false)

(define (solution? f lon)
  (equal? (build-list (length lon) f) lon))

;; Tests:
(check-expect (solution? (λ (n) (* 3 (add1 n))) '(3 6 9 12 15 18)) true)
(check-expect (solution? (λ (n) (* 3 (add1 n))) '(1 0 1 0 1 0)) false)
(check-expect (solution? identity empty) true)
(check-expect (solution? add1 empty) true)


;; (calculate-recursive n combine base) produces a list up to n of
;;   the terms of the recursive sequence specified by the combination
;;   function combine and the base-cases in base.
;; calculate-recursive: Nat (Nat Sample -> Int) Sample
;;                        -> Sample
;; requires: the cases in base are in reverse order, i.e. '(nk ... n0)
;; Examples:
(check-expect (calculate-recursive 9 (λ (n terms)
                                       (foldr + 0 terms)) '(1 1))
              (reverse test-07))
(check-expect (calculate-recursive 5 (λ (n terms)
                                       (* 2 (first terms)))
                                   '(4 2))
              (reverse test-04))

(define (calculate-recursive n combine base)
  (cond
    [(< n (length base)) (list-tail (add1 n) base)]
    [else (local [(define list (calculate-recursive (sub1 n)
                                                    combine
                                                    base))]
            (cons (combine n (list-head (length base) list))
                  list))]))


;; (guess-recursive combine base) produces a function that produces the
;;   terms of a recursive sequence defined by the combination rule combine,
;;   and the base cases base.
;; guess-recursive: (Nat Sample -> Int) Sample -> Solution
;; Examples:
(check-expect (solution? (guess-recursive (λ (n terms) n) '()) test-02) true)
(check-expect (solution? (guess-recursive (λ (n terms) n) '()) test-03) false)

(define (guess-recursive combine base)
  (λ (n) (first (calculate-recursive n combine base))))


;; ==== Prime Sequences ===================

;; (guess-prime i) produces a function that produces i when
;;   given 0, modelled as a prime-generating function.
;; guess-prime: Int -> Solution
;; Examples:
(check-expect (solution? (guess-prime 2) '(2 3 5 7 11 13)) true)
(check-expect (solution? (guess-prime 1) test-03) false)

(define (guess-prime i)
  (cond
    [(prime? i empty)
     (local [(define prime-index
               (index-of i (reverse (prime-list (floor (/ (add1 i) 2))))))]
       (λ (n)
         (first (prime-list (+ prime-index n 1)))))]
    [else (λ (n) 0)]))


;; ==== Constant Sequences ================

;; (guess-constant i) produces a function that produces i
;;   when given any value.
;; guess-constant: Int -> Solution
;; Examples:
(check-expect (solution? (guess-constant 1) test-05) true)
(check-expect (solution? (guess-constant 2) test-18) true)
(check-expect (solution? (guess-constant 1) test-03) false)

(define (guess-constant i)
  (λ (n) i))


;; ==== Linear Sequences ==================

;; (guess-linear i j) produces a function that produces i when
;;   given 0 and j when given 1, modelled as a linear function.
;; guess-linear: Int Int -> Solution
;; Examples:
(check-expect (solution? (guess-linear 2 4) test-01) true)
(check-expect (solution? (guess-linear 2 2) test-18) true)
(check-expect (solution? (guess-linear 1 11) test-03) false)

(define (guess-linear i j)
  (local [(define b i)
          (define a (- j i))]
    (λ (n) (+ (* a n) b))))


;; ==== Quadratic Sequences ===============

;; (guess-quadratic i j k) produces a function that produces i
;;   when given 0, j when given 1 and k when given 2, modelled as
;;   a quadratic curve.
;; guess-quadratic: Int Int Int -> Solution
;; Examples:
(check-expect (solution? (guess-quadratic 2 4 6) test-01) true)
(check-expect (solution? (guess-quadratic 0 1 2) test-02) true)
(check-expect (solution? (guess-quadratic 2 4 8) test-04) false)

(define (guess-quadratic i j k)
  (local [(define c i)
          (define b (/ (- (* 4 j) k (* 3 c)) 2))
          (define a (- j b c))]
    (λ (n) (+ (* (+ (* a n) b) n) c))))


;; ==== Polynomial Sequences ==============
;; Note: this is much slower than the constant, linear and
;;   quadratic patterns, so those should be used instead of
;;   a generic polynomial pattern whenever possible.

;; (calculate-differences depth lon) produces a list of the first
;;   element of each of the depthth differences of lon.
;; calculate-differences: Nat (listof Num) -> (listof Num)
;; Examples:
(check-expect (calculate-differences 3 '(1 2 3 4)) '(1 1))
(check-expect (calculate-differences 3 '(1 4 9)) '(1 3 2))
(check-expect (calculate-differences 3 empty) empty)
(check-expect (calculate-differences 1 '(1 4 9)) '(1 3))

(define (calculate-differences depth lon)
  (cond
    [(empty? lon) empty]
    [(empty? (rest lon)) (list (first lon))]
    [(zero? depth) (list (first lon))]
    [else (cons
           (first lon)
           (calculate-differences
            (sub1 depth)
            (foldr (λ (f s r-t)
                     (cond
                       [(and (cons? r-t) (= (- f s) (first r-t)))
                        r-t]
                       [else (cons (- f s) r-t)]))
                   empty
                   (rest lon)
                   (remove-last lon))))]))


;; (calculate-poly x diffs) produces the value of the polynomial
;;   described by the differences in diffs evaluated at x.
;; calculate-poly: Num (listof Num) -> Num
;; Examples:
(check-expect (calculate-poly 20 (calculate-differences 3 '(1 3 5)))
              41)
(check-expect (calculate-poly 20 empty) 0)

(define (calculate-poly x diffs)
  (local [;; (calculate-poly/acc x diffs n) produces the value of
          ;;   the polynomial described by the differences in diffs
          ;;   evaluated at x.
          ;; calculate-poly/acc: Num (listof Num) Nat -> Num
          ;; requires: n > 0
          (define (calculate-poly/acc x diffs n)
            (cond
              [(empty? diffs) 0]
              [else (+ (first diffs)
                       (* (- x (sub1 n))
                          (/ (calculate-poly/acc x (rest diffs)
                                                 (add1 n)) n)))]))]
    (calculate-poly/acc x diffs 1)))


;; (generate-poly-guesser i) produces a function that attempts to
;;   fit a polynomial of degree i to the input list.
;; generate-poly-guesser: Nat -> FullPattern
(define (generate-poly-guesser i)
  (λ (patterns lon)
    (local [(define diffs (calculate-differences i lon))]
      (λ (n)
        (calculate-poly n diffs)))))
(define quad (generate-poly-guesser 2))

;; ==== Linear Sum-Recursive Sequences ====

;; (guess-recursive-add i j k l) produces a function that produces i
;;   when given 0, j when given 1, k when given 2 and l when given 3,
;;   modelled as a linear additive recursive function.
;; guess-recursive-add: Int Int Int Int -> Solution
;; Examples:
(check-expect (solution? (guess-recursive-add 1 1 2 3) test-07) true)
(check-expect (solution? (guess-recursive-add 2 4 8 16) test-04) true)
(check-expect (solution? (guess-recursive-add 1 11 33 67) test-03) false)

(define (guess-recursive-add i j k l)
  (local [(define b-denom (- (sqr j) (* i k)))
          (define b (zero-divide (- (* j l) (sqr k)) b-denom))
          (define a (zero-divide (- l (* b j)) k))]
    (λ (n) (first (calculate-recursive n (λ (n terms)
                                           (+ (* a (first terms))
                                              (* b (second terms))))
                                       (list j i))))))


;; ==== Factorial Sequences ===============

;; (guess-! i j) produces a function that produces i when given
;;   0 and j when given 1, modelled as a factorial function.
;; guess-!: Int Int -> Solution
;; Examples:
(check-expect (solution? (guess-! 1 1) test-14) true)
(check-expect (solution? (guess-! 1 4) test-15) true)
(check-expect (solution? (guess-! 1 11) test-03) false)

(define (guess-! i j)
  (λ (n) (* i (expt (zero-divide j i) n) (! n))))


;; ==== Product-Recursive Sequences =======

;; (guess-recursive-prod i j k) produces a function that produces i
;;   when given 0, j when given 1 and k when given 2, modelled as a
;;   mutiplicative recursive function.
;; guess-recursive-prod: Int Int Int -> Solution
;; Examples:
(check-expect (solution? (guess-recursive-prod 1 2 2) test-12) true)
(check-expect (solution? (guess-recursive-prod 3 1 6) test-13) true)
(check-expect (solution? (guess-recursive-prod 1 11 33) test-03) false)

(define (guess-recursive-prod i j k)
  (local [(define a (zero-divide k (* i j)))]
    (λ (n) (first (calculate-recursive n (λ (n terms)
                                           (* a
                                              (first terms)
                                              (second terms)))
                                       (list j i))))))


;; ==== Look-and-Say Sequences ============

;; (guess-las i) produces a function that produces i when given 0,
;;   modelled as a look-and-say sequence (e.g. 1, 11, 21, 1211...).
;; guess-las Int -> Solution
;; Examples:
(check-expect (solution? (guess-las 2) '(2 12 1112 3112 132112)) true)
(check-expect (solution? (guess-las 3) test-13) false)

(define (guess-las i)
  (λ (n)
    (first (calculate-recursive n
                                (λ (n terms)
                                  (digits->nat (flatten
                                                (group-dup
                                                 (nat->digits
                                                  (first terms))))))
                                (list i)))))


;; ==== BONUS SOLUTION ====================

;; (try-double patterns lon) tries solving a given number sequence
;;   with all of the given patterns, by assuming that the input consists
;;   of another pattern's terms, doubled up.
;; try-double: (listof Pattern) Sample -> (anyof Solution empty)
(define (try-double patterns lon)
  (local [(define lst
            (foldl (λ (term rest)
                     (local [(define digits (nat->digits term))
                             (define count (length digits))
                             (define breakpoint
                               (floor (/ count 2.0)))
                             (define upper-breakpoint
                               (ceiling (/ count 2.0)))]
                       (append rest
                               (list (digits->nat
                                      (list-head
                                       breakpoint digits))
                                     (digits->nat
                                      (list-tail
                                       upper-breakpoint digits))))))
                   empty
                   lon))
          (define guess (try-all patterns lst))]
    (cond
      [(empty? guess) empty]
      [else (λ (n)
              (local [(define term1 (guess (* 2 n)))
                      (define term2 (guess (add1 (* 2 n))))]
                (+ (* term1 (expt number-base (digit-count term2)))
                   term2)))])))


;; (try-alt i) produces a function that tries solving a given number
;;   sequence with all of the given patterns, by assuming that the
;;   input alternates between i patterns.
;; try-alt: Nat -> FullPattern
(define (try-alt i)
  (λ (patterns lon)
    (local [(define parts (list-part i lon))
            (define output (foldr (λ (part result)
                                    (cond
                                      [(and (cons? result)
                                            (empty? (first result)))
                                       empty]
                                      [else (cons (try-all patterns part)
                                                  result)]))
                                  empty parts))]
      (cond
        [(empty? output) output]
        [else (λ (n)
                ((list-ref output (remainder n i)) (quotient n i)))]))))


(define (try-offset offset)
  (λ (patterns lon)
    (cond
      [(< offset 1) empty]
      [else (local [(define offset-recip (/ 1 offset))
                    (define (get-offset n)
                      (inexact->exact (* n (add1 (* 0.5 offset (sub1 n))))))
                    (define (get-order n)
                      (inexact->exact
                       (ceiling
                        (- (sqrt (+ 1/4
                                    (* (add1 (* 2 n)) offset-recip)
                                    (sqr offset-recip)))
                           1/2 offset-recip))))
                    (define (transform-input n)
                      (- n (get-offset (get-order n))))
                    (define (get-terms lon)
                      (local [(define len (length lon))
                              (define start
                                (get-offset (get-order (sub1 len))))
                              (define last-len (- len start))
                              (define second-len
                                (add1 (get-order (sub1 start))))]
                        (cond
                          [(< last-len second-len)
                           (list-head second-len
                                      (list-tail (+ second-len last-len) lon))]
                          [else (list-tail last-len lon)])))
                    (define solution
                      (try-all patterns (get-terms lon)))]
              (cond
                [(empty? solution) ((try-offset (sub1 offset)) patterns lon)]
                [else (λ (n)
                        (solution (transform-input n)))]))])))


(define (try-repeat depth)
  (λ (patterns lon)
    (cond
      [(< depth 2) empty]
      [else (local [(define solution
                      (try-all patterns (list-head depth lon)))]
              (cond
                [(empty? solution) ((try-repeat (sub1 depth)) patterns lon)]
                [else (local [(define result (λ (n)
                                               (solution
                                                (remainder n depth))))]
                        (cond
                          [(solution? result lon) result]
                          [else ((try-repeat (sub1 depth))
                                 patterns lon)]))]))])))


;; functions that may produce patterns that are solutions to sequences
(define ss-patterns (list (list guess-constant 1)
                          (list guess-linear 2)
                          (list guess-quadratic 3)
                          (list guess-recursive-add 4)
                          (list guess-! 2)
                          (list guess-recursive-prod 3)
                          (generate-poly-guesser 3)
                          (list guess-las 1)
                          (list guess-prime 1)
                          (try-offset 1)
                          (try-repeat 4)
                          try-double
                          (try-alt 2)
                          (try-offset 2)))


;; (try-all patterns lon) tries solving the number sequence lon with
;;   all of the given patterns. It produces the first solution that
;;   works and empty if it can't find one.
;; try-all: (listof Pattern) Sample -> (anyof Solution empty)
;; Examples:
(check-expect (solution? (try-all ss-patterns test-01) test-01)
              true)
;; (check-expect (try-all ss-patterns test-11) empty)

(define (try-all patterns lon)
  (foldl (λ (pattern result)
           (cond
             [(empty? result)
              (try-pattern patterns pattern lon)]
             [else result]))
         empty patterns))


;; (supersolve lon) tries solving the number sequence lon with
;;   many different patterns. It produces the solution if
;;   it works and empty if it can't find one. NOTE: See below
;;   for tests.
;; supersolve: Sample -> (anyof Solution empty)
(define (supersolve lon)
  (try-all ss-patterns lon))


;; (test-ss tests-to-run) produces true if supersolve produces correct
;;   results for all the tests in tests-to-run.
;; test-ss: (listof (list (listof Int) (listof Int))) -> Bool
(define (test-ss tests-to-run)
  (local [(define failed-tests
            (foldl (λ (test current-failed)
                     (local [(define full-test (append (first test)
                                                       (second test)))
                             (define first-guess (supersolve (first test)))
                             (define result
                               (and (solution? first-guess
                                               full-test)
                                    (solution? (supersolve full-test)
                                               full-test)))]
                       (cond
                         [result current-failed]
                         [else (cons test current-failed)])))
                   empty
                   tests-to-run))]
    failed-tests))


(check-expect (test-ss tests) empty)

(define (guess-n n func)
  (cond
    [(< n 0) '()]
    [else (append (guess-n (- n 1) func) (list (func n)))]))

(define (guess lon) (guess-n 10 (supersolve lon)))