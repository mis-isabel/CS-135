#lang eopl

#|-------------------------------------------------------------------------------
 | Name: Marguerite Sutedjo
 | Pledge: I pledge my honor that I have abided by the Stevens Honor System.
 |-------------------------------------------------------------------------------|#


#|-------------------------------------------------------------------------------|
 |                       Lab 10: Eulerian Cycles (20 PTS)                        |
 |-------------------------------------------------------------------------------|#


#| In this lab, we'll implement a test to determine whether
 |   an undirected graph contains an Eulerian cycle.
 | We'll use the following terminology for datatypes in the lab:
 | - A "natural" is an integer ≥ 0.
 | - A "vertex" is a natural.
 | - An "edge" is a list of two vertices.
 |   All edges in this lab are undirected.
 |   In other words, (1 2) and (2 1) are considered equivalent.
 | - A "graph" is a list of edges.
 |   All graphs in this lab are undirected.
 |   Empty graphs ARE always valid input for any function in the lab.
 |   You MAY assume that a graph does not contain duplicates edges,
 |     including symmetric edges.
 |   In other words, if (1 2) is listed as an edge of a graph,
 |     the list of edges will not contain another instance of (1 2),
 |     NOR will it contain any instances of (2 1).
 |
 | As a refresher, an Eulerian cycle is a cycle which traverses
 |   every edge of a graph exactly once.
 | Given an arbitrary undirected graph G,
 |   G contains an Eulerian cycle iff it satisfies two conditions:
 |   1. The degree of every vertex in G is even.
 |   2. G is connected, save any vertices of degree 0.
 |#


;; Below are some example graphs we'll use throughout the lab for testing.
;; You may want to draw these out to better understand what the expected outputs should be.

(define C6 '((1 2) (2 3) (3 4) (4 5) (5 6) (6 1)))
(define K4 '((4 3) (4 2) (4 1) (3 2) (3 1) (2 1)))
(define full5 '((1 1) (1 2) (1 3) (1 4) (1 5)
                      (2 2) (2 3) (2 4) (2 5)
                      (3 3) (3 4) (3 5)
                      (4 4) (4 5) (5 5)))
(define btree '((00 10) (00 11) (10 20) (10 21) (11 22) (11 23)
                        (20 30) (20 31) (21 32) (21 33)
                        (22 34) (22 35) (23 36) (23 37)))
(define triforce '((00 10) (00 11) (10 11) (10 20) (10 21)
                           (20 21) (11 21) (11 22) (21 22)))
(define polygons '((1 2) (2 3) (3 4) (4 1)
                         (5 6) (6 7) (7 5)))
(define reflex '((1 1) (2 2) (3 3)))
(define empty '())




#|-------------------------------------------------------------------------------|
 |                               Helper Functions                                |
 |-------------------------------------------------------------------------------|#

;; Provided in this section are some helper functions.
;; You MAY and should use these anywhere you want in your implementations.


#| "remove" accepts lists L and R
 |   and returns L filtered such that all elements of L
 |   which are also in R have been removed.
 |#
;; Type signature: (remove list list) -> list
(define (remove L R)
  (define (recurse acc L)
    (if (null? L) (reverse acc)
        (recurse (if (member (car L) R)
                     acc
                     (cons (car L) acc))
                 (cdr L))))
  (recurse '() L))




#| "usort" accepts a list of naturals L
 |   and returns L sorted from least to greatest
 |   with all duplicate elements removed (hence "unique-sort").
 |#
;; Type signature: (usort list-of-naturals) -> list-of-naturals
(define (usort L)	
  (define (recurse acc L)	
    (if (null? L) acc
        (let ([m (apply max L)])	
          (recurse (cons m acc) (remove L (list m))))))	
  (recurse '() L))




#| "dfs" accepts a graph G and vertex v
 |   and returns a list of all vertices accessible from v
 |   via some sequence of edges in G.
 | The returned list of vertices may be unsorted and may contain duplicates.
 | This function requires a working implementation of the function "adjacent",
 |   which you'll implement later.
 |#
;; Type signature: (dfs graph vertex) -> list-of-vertices
(define (dfs G v)
  (define (helper found v)
    (define next-found (cons v found))
    (define adj (remove (adjacent G v) next-found))
    (define (f x) (helper next-found x))
    (define res (apply append (map f adj)))
    (cons v res))
  (helper '() v))




#|-------------------------------------------------------------------------------|
 |                        Part 1: Condition #1                   |
 |-------------------------------------------------------------------------------|#


#|
 | Paste your functions from Part 1 here!!!
 |#


#| First, let's implement a test for whether a graph satisfies
 |   the first condition necessary for an Eulerian cycle to exist:
 |   is the degree of every vertex even?
 |#


#| Implement "get-vertices" to accept a graph G
 |   and return a list of all unique vertices in G
 |   sorted from least to greatest.
 | Because we are encoding graphs as a list of edges,
 |   this function's output will necessarily exclude
 |   vertices of degree 0, but we'll ignore that fact
 |   since we have no way of representing vertices of degree 0,
 |   and they are irrelevant to Eulerian cycles.
 | Make sure your output matches the expect output exactly -
 |   no duplicates, and sorted from least to greatest.
 |
 | Examples:
 |   (get-vertices C6)       -> (1 2 3 4 5 6)
 |   (get-vertices K4)       -> (1 2 3 4)
 |   (get-vertices full5)    -> (1 2 3 4 5)
 |   (get-vertices btree)    -> (0 10 11 20 21 22 23 30 31 32 33 34 35 36 37)
 |   (get-vertices triforce) -> (0 10 11 20 21 22)
 |   (get-vertices polygons) -> (1 2 3 4 5 6 7)
 |   (get-vertices reflex)   -> (1 2 3)
 |   (get-vertices empty)    -> ()
 |#

;; Type signature: (get-vertices graph) -> list-of-vertices
;; 3 PTS
(define (get-vertices G)
  (usort (gvHelper G)))

(define (gvHelper G)
  (cond [(null? G) empty]
        [(append (gvHelper (cdr G)) (car G))]))


#| Implement "degree" to accept a graph G and a vertex v,
 |   and return the degree of v in G.
 | The degree of v in G is the total number of edges in G
 |   which point into OR out of v.
 | Reflexive edges - edges of the form (v v) - are special,
 |   in that they add 2 to the degree of v.
 |
 | Again, you may assume G does not contain duplicate edges.
 |
 | Examples:
 |   (degree C6 3)        -> 2
 |   (degree C6 7)        -> 0
 |   (degree K4 4)        -> 3
 |   (degree full5 2)     -> 6
 |   (degree btree 21)    -> 3
 |   (degree triforce 10) -> 4
 |   (degree polygons 6)  -> 2
 |   (degree reflex 2)    -> 2
 |   (degree empty 1)     -> 0
 |#

;; Type signature: (degree graph vertex) -> natural
;; 4 PTS
(define (degree G v)
  (cond [(null? G) 0]
        [(and (equal? (cadar G) v) (equal? (caar G) v)) (+ 2 (degree (cdr G) v))]
        [(equal? (caar G) v) (+ 1 (degree (cdr G) v))]
        [(equal? (cadar G) v) (+ 1  (degree (cdr G) v))]
        [else (degree (cdr G) v)]))


#| Implement "all-even?" to accept a graph G
 |   and return whether all of G's vertices have even degree.
 | Notice that "all" of the empty graph's vertices have even degree.
 |
 | Examples:
 |   (all-even? C6)       -> #t
 |   (all-even? K4)       -> #f
 |   (all-even? full5)    -> #t
 |   (all-even? btree)    -> #f
 |   (all-even? triforce) -> #t
 |   (all-even? polygons) -> #t
 |   (all-even? reflex)   -> #t
 |   (all-even? empty)    -> #t
 |#

;; Type signature: (all-even? graph) -> boolean
;; 3 PTS
(define (all-even? G)
  (allEvenHelper G (get-vertices G)))

(define (allEvenHelper G v)
  (cond [(null? v) #t]
        [(odd? (degree G (car v))) #f]
        [else (allEvenHelper G (cdr v))]))




#|-------------------------------------------------------------------------------|
 |                         Part 2: Condition #2 (4 EC PTS)                          |
 |-------------------------------------------------------------------------------|#


#| Now we'll implement a test for whether a graph satisfies
 |   the second condition necessary for an Eulerian cycle to exist:
 |   is the graph connected?
 |
 | An undirected graph is connected if a path (a sequence of edges) exists
 |   from every vertex to every other vertex.
 | Again, the exception we're making to this rule is to ignore
 |   "isolated" vertices of degree 0, since they do not effect
 |   the existence of an Eulerian cycle.
 |#


#| 
 | Paste your solution for adjacent here!
 |#

;; Type signature: (adjacent graph vertex) -> list-of-vertices
;; 4 PTS
(define (adjacent G v)
  (usort (adjacentHelper G v)))

(define (adjacentHelper G v)
  (cond [(null? G) empty]
        [(equal? (caar G) v) (append (adjacentHelper (cdr G) v) (cdar G))]
        [(equal? (cadar G) v) (append (adjacentHelper (cdr G) v) (list(caar G)))]
        [else (adjacentHelper (cdr G) v)]))



#| Implement "connected?" to accept a graph G
 |   and return whether G is connected.
 | Take advantage of the "dfs" helper function,
 |   as it will return all of the vertices reachable
 |   from some starting vertex.
 | Given that G is undirected, if it is connected
 |   you should be able to start from any initial vertex
 |   and reach every other vertex.
 | So, find a way to select any arbitrary vertex
 |   from G, and invoke "dfs" with that vertex as the initial vertex.
 | If the graph is connected, the output of the depth-first search
 |   will contain all the vertices in the graph.
 | Consider, given the provided functions,
 |   how you can easily compare the contents of the "dfs" output
 |   with G's list of vertices.
 |
 | Examples:
 |   (connected? C6)       -> #t
 |   (connected? K4)       -> #t
 |   (connected? full5)    -> #t
 |   (connected? btree)    -> #t
 |   (connected? triforce) -> #t
 |   (connected? polygons) -> #f
 |   (connected? reflex)   -> #f
 |   (connected? empty)    -> #t
 |#

;; Type signature: (connected? graph) -> boolean
;; 4 PTS
(define (connected? G)
  (cond [(or (null? G) (equal? (usort(dfs G (caar G))) (get-vertices G))) #t]
        [else #f]))

#|-------------------------------------------------------------------------------|
 |                          Part 3: The Finale (2 EC PTS)                           |
 |-------------------------------------------------------------------------------|#


#| Implement "eulerian?", which accepts a graph G
 |   and returns whether G contains an Eulerian cycle
 |   (a.k.a. G is an "Eulerian graph").
 | Use the two conditions we've now implemented
 |   and check that G satisfies both.
 |
 | Examples:
 |   (eulerian? C6)       -> #t
 |   (eulerian? K4)       -> #f
 |   (eulerian? full5)    -> #t
 |   (eulerian? btree)    -> #f
 |   (eulerian? triforce) -> #t
 |   (eulerian? polygons) -> #f
 |   (eulerian? reflex)   -> #f
 |   (eulerian? empty)    -> #t
 |#

;; Type signature: (eulerian? graph) -> boolean
;; 2 PTS
(define (eulerian? G)
  (and (all-even? G) (connected? G)))