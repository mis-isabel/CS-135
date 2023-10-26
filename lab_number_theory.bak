#lang eopl

#|-------------------------------------------------------------------------------
 | Name:
 | Pledge:
 |-------------------------------------------------------------------------------|#


#|-------------------------------------------------------------------------------|
 |                        Lab 8: Number Theory (20 PTS)                          |
 |-------------------------------------------------------------------------------|#


#| This lab explores a few topics of number theory
 |   that have been covered in lecture thus far.
 | Some functions you'll write may come in handy for subsequent ones,
 |   while others are "standalone".
 |
 | The type "natural" refers to a nonnegative integer >= 0.
 | The type "posint" refers to a positive integer > 0.
 | You'll find the following functions built-in to EOPL useful:
 |   (modulo a b)   returns a mod b [a % b in Python].
 |   (quotient a b) returns the integer quotient of a ÷ b [a // b in Python].
 |#




#|-------------------------------------------------------------------------------|
 |                   Part 1: Greatest Common Divisor (10 PTS)                     |
 |-------------------------------------------------------------------------------|#


#| Implement "divides?" to accept naturals a and b
 |   and return whether a|b, or "a divides b".
 | If a is zero, a divides b when b is zero.
 | If a is not zero, a divides b when b % a is zero.
 |
 | "divides?" behaves the same as "p-divides?" from the peano lab,
 |   but now you can utilize all the "normal" integer arithmetic we're accustomed to.
 |
 | Examples:
 |   (divides? 0 0)  -> #t
 |   (divides? 0 3)  -> #f
 |   (divides? 3 0)  -> #t
 |   (divides? 1 7)  -> #t
 |   (divides? 7 2)  -> #f
 |   (divides? 8 8)  -> #t
 |   (divides? 3 12) -> #t
 |   (divides? 4 18) -> #f
 |#

;; Type signature: (divides? natural natural) -> boolean
;; 4 PTS
(define (divides? a b)
  "TODO: Implement")




#| Implement "gcd" to accept two naturals
 |   and return their greatest common divisor.
 | Compute the GCD with Euclid's algorithm, which recursively uses
 |   the property that gcd(a, b) = gcd(b, a mod b).
 | Do NOT use EOPL's built-in "gcd" function!
 | You may assume that at least one of the two inputs to "gcd" is not zero.
 |
 | Examples:
 |   (gcd 0 1)   -> 1
 |   (gcd 1 5)   -> 1
 |   (gcd 3 2)   -> 1
 |   (gcd 4 8)   -> 4
 |   (gcd 18 16) -> 2
 |   (gcd 20 20) -> 20
 |   (gcd 65 25) -> 5
 |   (gcd 24 78) -> 6
 |   (gcd 95 47) -> 1
 |#

;; Type signature: (gcd natural natural) -> natural
;; 6 PTS
(define (gcd a b)
  "TODO: Implement")




#|-------------------------------------------------------------------------------|
 |                    Part 2: The Water Jug Problem (10 PTS)                      |
 |-------------------------------------------------------------------------------|#


#| In this section, we'll programmatically solve the water jug problem.
 | First, let's lay out the problem's specification.
 |
 | Suppose there are two jugs, jug A and jug B,
 |   which have respective capacities of cA and cB gallons.
 | The capacity of each jug is a positive integer.
 | An arbitrarily large pool of water is also available
 |   for filling and emptying the jugs.
 | The objective of the water jug problem is to perfectly measure
 |   some integer amount of gallons, g, by performing a sequence of steps
 |   in which the jugs can be emptied, filled, or poured into one another.
 | Jugs A and B begin empty, and the problem has been solved
 |   once one of the jugs contains exactly g gallons of water.
 |#




#| Implement "wj-solvable?" to accept posints cA, cB, and g,
 |   and return whether g gallons can be measured from
 |   two jugs A and B with capacities of cA and cB gallons respectively.
 | g can be measured when g is a multiple of
 |   the greatest common divisor of the capacities of the two jugs,
 |   and, of course, when one of the jugs can hold at least g gallons.
 |
 | Examples:
 |   (wj-solvable? 1 1 1)   -> #t
 |   (wj-solvable? 1 2 3)   -> #f
 |   (wj-solvable? 1 10 2)  -> #t
 |   (wj-solvable? 1 10 9)  -> #t
 |   (wj-solvable? 2 4 3)   -> #f
 |   (wj-solvable? 6 12 12) -> #t
 |   (wj-solvable? 6 12 18) -> #f
 |   (wj-solvable? 7 8 5)   -> #t
 |   (wj-solvable? 9 12 6)  -> #t
 |#

;; Type signature: (wj-solvable? posint posint posint) -> boolean
;; 3 PTS
(define (wj-solvable? cA cB g)
  "TODO: Implement")


  

#| Implement "waterjug" to accept posints cA, cB, and g,
 |   and solve the waterjug problem with jugs A and B
 |   [with respective capacities of cA and cB]
 |   and the objective of measuring g gallons.
 | The output of "waterjug" represents the sequence of amounts of water
 |   in both jugs at each step of the process.
 |
 | You may make the following assumptions about the input to "waterjug":
 |   1. cA <= cB.
 |   2. g gallons can be measured with jugs of capacities cA and cB.
 |      Before trying your own test cases with this function,
 |        validate them by hand with "wj-solvable?".
 |
 | Both jugs are initially empty.
 | At each step of the algorithm, one of the following five cases will be true,
 |   where fA is the contents of jug A and fB is the contents of jug B.
 | Whichever is the first true case should be executed:
 |   1. fB = g       -> Process is done!
 |   2. fB = cB      -> Completely empty jug B.
 |   3. fA = 0       -> Completely fill jug A.
 |   4. fA + fB > cB -> Pour from jug A into jug B until B is full.
 |   5. fA + fB ≤ cB -> Completely empty jug A into jug B.
 | Though there are other algorithms which solve this problem more efficiently,
 |   it's important that you follow the process above so your output matches the expected output.
 | The output is a list of pairs of integers,
 |   where the nth pair is the values of (fA fB) at the nth step of the process.
 |
 | Examples:
 |   (waterjug 2 2 2)   -> ((0 0) (2 0) (0 2))
 |   (waterjug 3 5 4)   -> ((0 0) (3 0) (0 3) (3 3) (1 5) (1 0) (0 1) (3 1) (0 4))
 |   (waterjug 10 12 6) -> ((0 0) (10 0) (0 10) (10 10) (8 12) (8 0) (0 8) (10 8) (6 12) (6 0) (0 6))
 |   (waterjug 13 17 9) -> ((0 0) (13 0) (0 13) (13 13) (9 17) (9 0) (0 9))
 |   (waterjug 1 7 4)   -> ((0 0) (1 0) (0 1) (1 1) (0 2) (1 2) (0 3) (1 3) (0 4))
 |   (waterjug 9 18 18) -> ((0 0) (9 0) (0 9) (9 9) (0 18))
 |#

;; Type signature: (waterjug posint posint posint) -> list-of-pairs
;; 6 PTS
(define (waterjug cA cB g)
  "TODO: Implement")
