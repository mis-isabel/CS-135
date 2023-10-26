#lang eopl

#|-------------------------------------------------------------------------------
 | Name: Marguerite Sutedjo
 | Pledge: I pledge my honor that I have abided by the Stevens Honor System.
 |-------------------------------------------------------------------------------|#


#|-------------------------------------------------------------------------------|
 |                       Lab ^: Peano Arithmetic (20 PTS)                        |
 |-------------------------------------------------------------------------------|#


#| In this lab, we'll implement Peano arithmetic in Racket.
 |
 | Peano arithmetic defines the natural numbers inductively.
 | Zero is the first natural number, followed by its successor (one),
 |   followed by that number's successor (two),
 |   then that number's successor (three),
 |   and so on for infinitely many numbers.
 |
 | To represent the natural numbers as these successive "peano numbers",
 |   we want a way to symbolize this inductive (recursive!) process.
 | We'll use nested lists to represent the naturals
 |   because it is convenient, but ultimately
 |   the representation we choose is arbitrary.
 |
 | We begin by defining our base case, zero, as the empty list:
 |#

(define z '() )

#| Now we can inductively define the peano numbers:
 |   Given a peano number p, the next number (successor)
 |   is one level of nesting deeper than p.
 | Here are the first few peano numbers using this representation:
 |   0 = ()
 |   1 = (())
 |   2 = ((()))
 |   3 = (((())))
 |   ... and so on ad infinitum.
 |
 | In the type signatures and descriptions of this lab,
 |   "natural" refers to a nonnegative integer, and
 |   "peano" refers to a peano number represented as a nested list.
 |#




#|-------------------------------------------------------------------------------|
 |                                Error Handling                                 |
 |-------------------------------------------------------------------------------|#

#| We're going to introduce error handling in this lab.
 | Some functions we implement will have certain cases
 |   where they need to fail, at which point we'll raise/invoke an error.
 | If a test case states that a function's expected output is,
 |   for example, <error-conversion>, that does NOT mean that "<error-conversion>"
 |   should literally be the output; rather, (error-conversion) should be invoked.
 |
 | Below are a set of specific errors which we'll use at various points in the lab.
 | To see what happens when these errors are invoked,
 |   try invoking one in the terminal with, for example, (error-conversion).
 |#

(define (error-conversion)  (eopl:error "Error converting to peano: argument cannot be negative!"))
(define (error-predecessor) (eopl:error "Predecessor error: zero has no predecessor!"))
(define (error-subtraction) (eopl:error "Subtraction error: a < b!"))
(define (error-modulo)      (eopl:error "Modulo error: mod is zero!"))




#|-------------------------------------------------------------------------------|
 |                               Testing Functions                               |
 |-------------------------------------------------------------------------------|#

#| In this section are helper functions strictly for
 |   aiding you in testing your code.
 | YOU MAY NOT USE THESE FUNCTIONS IN YOUR IMPLEMENTATIONS!
 | Your grade for each function will be penalized
 |   if you use any testing functions in your implementations.
 |#


#| "peano?" accepts any datatype and returns
 |   whether it is a properly formatted peano number
 |   [a nested list with no extra items anywhere].
 | Examples:
 |   (peano? '() )         -> #t
 |   (peano? '(((()))) )   -> #t
 |   (peano? 5)            -> #f
 |   (peano? "(())" )      -> #f
 |   (peano? '(5 (() 3)) ) -> #f
 |#
;; Type signature: (peano? any) -> boolean
(define (peano? p)
  (cond
    [(not (list? p)) #f]
    [(= 0 (length p)) #t]
    [(= 1 (length p))
     (peano? (car p))]
    [else #f]))


#| "n->p" converts a natural number to a peano number,
 |   and raises an error upon receiving a negative number.
 | Examples:
 |   (n->p -1) -> <error-conversion>
 |   (n->p 0)  -> ()
 |   (n->p 2)  -> ((()))
 |   (n->p 5)  -> (((((())))))
 |#
;; Type signature: (n->p natural) -> peano
(define (n->p n)
  (cond
    [(zero? n) z]
    [(positive? n)
     (list (n->p (- n 1)))]
    [else (error-conversion)]))


#| "p->n" converts from a peano number to a natural number.
 | Examples:
 |   (p->n '() )           -> 0
 |   (p->n '((())) )       -> 2
 |   (p->n '(((((()))))) ) -> 5
 |#
;; Type signature: (p->n peano) -> natural
(define (p->n p)
  (if (null? p) 0
      (+ 1 (p->n (car p)))))


#| "tst1" accepts a unary peano function and a natural,
 |   converts the natural to peano, and runs the provided function.
 | If the function's output is a peano number,
 |   tst1 converts it back to a natural before outputting the result.
 |#
;; Type signature: (tst1 unary-function natural) -> natural or boolean
(define (tst1 func a)
  (let ([res (func (n->p a))])
    (if (peano? res) (p->n res) res)))


#| "tst2" accepts a binary peano function and two naturals,
 |   converts the naturals to peanos, and runs the provided function.
 | If the function's output is a peano number,
 |   tst2 converts it back to a natural before outputting the result.
 |#
;; Type signature: (tst2 binary-function natural natural) -> natural or boolean
(define (tst2 func a b)
  (let ([res (func (n->p a) (n->p b))])
    (if (peano? res) (p->n res) res)))




#|-------------------------------------------------------------------------------|
 |                    Part 1: Atomic Peano Operations (3 PTS)                    |
 |-------------------------------------------------------------------------------|#


#| First, we must implement some fundamental operations on peano numbers.
 | From these operations, we can build more complex ones.
 | Throughout the lab,
 |   you may NOT use any integer literals in your function implementations,
 |   such as 0 and 1.
 | You grades for functions will be penalized for each violation of this restriction.
 |#




#| Implement "pz?" to accept a peano p and return whether p is zero.
 | Do this by comparing p to the representation of zero we defined above, "z".
 |
 | Examples:
 |   (pz? '() )    -> #t
 |   (pz? '(()) )  -> #f
 |   (tst1 pz? 0)  -> #t
 |   (tst1 pz? 1)  -> #f
 |   (tst1 pz? 40) -> #f
 |#

;; Type signature: (pz? peano) -> boolean
;; 2 PTS
(define (pz? p)
  (if (equal? z p)
      #t
      #f))




#| Implement "succ" to accept a peano number p and return the successor of p.
 | Recall that in our representation of peano arithmetic,
 |   p's successor is one level of nesting deeper than p.
 |
 | Examples:
 |   (succ '(()) )     -> ((()))
 |   (succ '(((()))) ) -> ((((()))))
 |   (tst1 succ 0)     -> 1
 |   (tst1 succ 5)     -> 6
 |   (tst1 succ 21)    -> 22
 |   (p->n (succ (succ (n->p 3)))) -> 5
 |#

;; Type signature: (succ peano) -> peano
;; 2 PTS
(define (succ p)
  (list p))




#| Implement "pred" to accept a peano p and return the predecessor of p.
 | Recall that in our representation of peano arithmetic,
 |   p's predecessor is one level of nesting shallower than p.
 |
 | One of the Peano axioms states that zero has no predecessor.
 | Therefore, if the input to "pred" is 0,
 |   we'll raise the "predecessor error" declared in the "error handling" section.
 | Do this with the following function call: (error-predecessor)
 |
 | Examples:
 |   (pred '((())) ) -> (())
 |   (pred '() )     -> <error-predecessor>
 |   (tst1 pred 1)   -> 0
 |   (tst1 pred 38)  -> 37
 |   (p->n (pred (pred (pred (n->p 14))))) -> 11
 |#

;; Type signature: (pred peano) -> peano
;; 2 PTS
(define (pred p)
  (cond [(pz? p) (error-predecessor)]
        [else (car p)]))


#|-------------------------------------------------------------------------------|
 |                   Part 2: Basic Arithmetic Operations (11 PTS)                |
 |-------------------------------------------------------------------------------|#


#| Now that we have the basic operations of
 |   succession, predecession, and comparison with zero,
 |   we can implement other common arithmetic operations.
 | From this point forward,
 |   the fact that we represent peanos with nested lists
 |   becomes completely irrelevant, because the use of lists
 |   has been abstracted away with "z", "succ", "pred", and "pz?".
 | Pretend you don't know we're using nested lists:
 |   Do NOT use cdr, car, list, null?, or any other list-related functions.
 |   Do NOT use any built-in comparison functions,
 |     such as equal?, =, eq?, <, >=, etc.
 |   Do NOT use any list literals such as '((())).
 |     For that example, you'd instead use (succ (succ z)).
 | Your grades for each function will be penalized
 |   in proportion to the points they're worth
 |   upon violations of these rules.
 |#




#| Implement "p+" to accept two peanos and return their sum as a peano.
 | You may want to review Monday's lecture to see the recursive
 |   definition of addition, which utilizes the succession and predecession operations.
 | Again, do NOT convert in and out of the peano form to perform the addition!
 |
 | Examples:
 |   (p+ '((())) '(()) ) -> (((())))
 |   (tst2 p+ 0 0) -> 0
 |   (tst2 p+ 5 4) -> 9
 |   (tst2 p+ 1 6) -> 7
 |   (tst2 p+ 0 10) -> 10
 |#

;; Type signature: (p+ peano peano) -> peano
;; 2 PTS
(define (p+ a b)
  (cond [(pz? b) a]
        [else (p+ (succ a) (pred b))]))




#| Implement "p-" to accept peanos a and b
 |   and return their difference, a - b, as a peano.
 | This operation has a catch: since there are no negative peanos,
 |   we can't compute a - b if a < b.
 | If this problem occurs, raise the subtraction error declared above
 |   by calling (error-subtraction).
 | Since we haven't implemented a "less than" operator for peanos,
 |   you'll need to get creative with how to
 |   recognize when this error should occur.
 |
 | Examples:
 |   (p- '((((())))) '((())) ) -> ((()))
 |   (tst2 p- 12 12) -> 0
 |   (tst2 p- 6 4)   -> 2
 |   (tst2 p- 4 6)   -> <error-subtraction>
 |   (tst2 p- 30 19) -> 11
 |   (tst2 p- 10 0)  -> 10
 |#

;; Type signature: (p- peano peano) -> peano
;; 2 PTS
(define (p- a b)
  (cond [(and (pz? a) (not (pz? b))) (error-subtraction)]
        [(pz? b) a]
        [else (p- (pred a) (pred b))]))




#| Implement "p*" to accept two peanos and return their product as a peano.
 | Recall Monday's definition of peano multiplication, along with
 |   how you first learned multiplication in elementary school:
 |   multiplication is just repeated addition!
 | Taking advantage of functions you've already written
 |   will make writing this one much easier.
 |
 | Examples:
 |   (p* '(((()))) '((())) ) -> ((((((()))))))
 |   (tst2 p* 0 0)  -> 0
 |   (tst2 p* 3 0)  -> 0
 |   (tst2 p* 4 3)  -> 12
 |   (tst2 p* 1 7)  -> 7
 |   (tst2 p* 17 5) -> 85
 |#

;; Type signature: (p* peano peano) -> peano
;; 2 PTS
(define (p* a b)
  (cond [(pz? b) z]
        [else (p+ a(p* a (pred b)))]))




#| Implement "p^" to accept peano numbers a and b
 |   and return a^b as a peano number.
 | You should find the implementation of p^
 |   to be very similar to that of p*,
 |   but instead of repeated addition,
 |   exponentiation is repeated ___________!
 |
 | Be careful when testing this function with larger numbers.
 | The result can get big really quickly,
 |   and our implementation with nested lists and recursion
 |   won't take too kindly to large computations.
 |
 | Examples:
 |   (p^ '((())) '(((()))) ) -> ((((((((()))))))))
 |   (tst2 p^ 0 4) -> 0
 |   (tst2 p^ 5 0) -> 1
 |   (tst2 p^ 4 2) -> 16
 |   (tst2 p^ 3 3) -> 27
 |   (tst2 p^ 2 7) -> 128
 |#

;; Type signature: (p^ peano peano) -> peano
;; 1 PTS
(define (p^ a b)
  (cond [(pz? b) (succ b)]
        [else (p* a(p^ a (pred b)))]))




#| Implement "p=" to accept two peano numbers
 |   and return whether they are equal.
 | Again, you may NOT use any of Racket's built-in equality functions.
 | The only function we have for comparing peano numbers is "pz?".
 | Once you've implemented p=, feel free to use it
 |   to directly compare peanos in subsequent functions.
 |
 | Examples:
 |   (p= '(()) '((())) ) -> #f
 |   (p= '(((()))) '(((()))) ) -> #t
 |   (tst2 p= 4 3) -> #f
 |   (tst2 p= 5 5) -> #t
 |   (tst2 p= 0 0) -> #t
 |#

;; Type signature: (p= peano peano) -> boolean
;; 2 PTS
(define (p= a b)
  (cond [(or (pz? a) (pz? b)) (if (and (pz? a) (pz? b)) #t
                     #f)]
        [else (p= (pred a) (pred b))]))

        
      
      




#| Implement "p>" to accept peano numbers a and b
 |   and return whether a > b.
 | Again, this isn't trivial because you can't simply
 |    convert the inputs to regular integers and
 |    compare them with the built-in > operator.
 |
 | Examples:
 |   (p> '((())) '(()) ) -> #t
 |   (tst2 p> 4 6)       -> #f
 |   (tst2 p> 3 3)       -> #f
 |   (tst2 p> 7 2)       -> #t
 |#

;; Type signature: (p> peano peano) -> boolean
;; 2 PTS
(define (p> a b)
  (cond [(or (pz? a) (pz? b)) (if (and (pz? b) (not (pz? a))) #t
                     #f)]
        [else (p> (pred a) (pred b))]))




#|-------------------------------------------------------------------------------|
 |                 Part 3: Number Theoretic Operations (6 PTS)                   |
 |-------------------------------------------------------------------------------|#


#| Here, we'll explore some operations essential to number theory,
 |   before studying them more thoroughly as the semester progresses.
 | The restrictions established in Parts 1 and 2 regarding the use
 |   of integer literals, list literals, list functions,
 |   and built-in comparison functions still apply here.
 |#




#| Implement "p%" to accept peano numbers a and b
 |   and return a mod b, or "a % b".
 | a % b is the remainder of dividing a by b.
 | Remember: integer division is repeated ____________!
 | The modulo operator is undefined when the divisor is zero,
 |   so p% should invoke (error-modulo) when b is zero.
 |
 | Examples:
 |   (p% '(((((()))))) '(((()))) ) -> ((()))
 |   (tst2 p% 6 0)    -> <error-modulo>
 |   (tst2 p% 3 5)    -> 3
 |   (tst2 p% 0 6)    -> 0
 |   (tst2 p% 30 15)  -> 0
 |   (tst2 p% 8 1)    -> 0
 |   (tst2 p% 28 8)   -> 4
 |   (tst2 p% 124 17) -> 5
 |#

;; Type signature: (p% peano peano) -> peano
;; 2 PTS
(define (p% a b)
  (cond [(p> b a) a]
        [(pz? b) (error-modulo)]
        [else (p% (p- a b) b)]))


#| Implement "p-divides?" to accept peanos a and b
 |   and return whether a|b, or "a divides b".
 | If a is not zero, a divides b when b % a is zero.
 | If a is zero, a divides b when b is zero.
 |
 | Examples:
 |   (p-divides? '((())) '(((()))) ) -> #f
 |   (tst2 p-divides? 0 0)  -> #t
 |   (tst2 p-divides? 0 3)  -> #f
 |   (tst2 p-divides? 3 0)  -> #t
 |   (tst2 p-divides? 1 7)  -> #t
 |   (tst2 p-divides? 7 2)  -> #f
 |   (tst2 p-divides? 3 12) -> #t
 |   (tst2 p-divides? 4 18) -> #f
 |#

;; Type signature: (p-divides? peano peano) -> boolean
;; 1 PTS
(define (p-divides? a b)
  (cond [(and (pz? a) (pz? b) #t)]
        [(and (not (pz? a)) (pz? (p% b a)) #t)]
        [else #f]))



#| EXTRA CREDIT:
 | Implement "p-prime?" to accept a peano number p
 |   and return whether p is a prime number.
 | p is prime if it is not divisible by any numbers
 |   between 2 inclusive and p exclusive.
 | Zero and one are not prime.
 | You'll likely need a helper function for this one!
 |
 | Examples:
 |   (p-prime? '(((((((()))))))) ) -> #t
 |   (tst1 p-prime? 0)  -> #f
 |   (tst1 p-prime? 2)  -> #t
 |   (tst1 p-prime? 5)  -> #t
 |   (tst1 p-prime? 9)  -> #f
 |   (tst1 p-prime? 35) -> #f
 |   (tst1 p-prime? 37) -> #t
 |#

;; Type signature: (p-prime? peano) -> boolean
;; 3 EC PTS]
(define (primeHelp a b)
  (cond [(p> (succ(succ z)) b) #t]
        [(pz? b) #f]
        [(pz? (p% a b)) #f]
        [else (primeHelp a (pred b))]))

(define (p-prime? p)
  (cond [(pz? p) #f]
        [else (primeHelp p (pred p))]))