#lang racket
;
(provide
 hours
 look-up
 repeat-key?
 bor band bnot
 get-type
 eval-in-env
 all-lists
 all-combs
 satisfiable
 convert-bnew
 
 )
; DO NOT CHANGE THE LINES ABOVE THIS COMMENT
; Name: Isaac Pena
; Email address: isaac.pena@yale.edu

; ** problem 0 ** (1 point)
; Modify the following procedure to return the number of
; hours you spent on this assignment (including reading):

(define hours 12)

; CS 201a HW #3  DUE Monday, October 6, 2014 at 11:59 pm, 
; electronically, using the submit command.  
; Do *not* email your homework -- lateness penalties will accrue 
; until you successfully submit it using your Zoo account.

; Only use procedures we have used in class, unless the problem
; specifically authorizes the use of Racket procedure
; in the problem statement.


; We define a table as a list of entries, where 
; each entry is a list of two items: the key and the value
; Examples of tables

(define table1 '((a 1) (large 5) (zot 3)))
(define table2 '((2 prime) (3 prime) (2 even)))
(define table3 '((20 2) (10 1) (40 3) (20 2)))

; ** problem 1 ** (9 points)
; Write a procedure (lookup item table)
; that returns a list consisting of two elements:
; the first and second elements are both #f if no
; entry in the table has a key equal to item;
; otherwise, the first element is #t and the second
; element is the value paired with the given key
; (use the earliest entry in the table if several
; have the given key.)

; Examples
; (look-up 'a table1) => '(#t 1)
; (look-up 'zot table1) => '(#t 3)
; (look-up 'fir table1) => '(#f #f)
; (look-up 2 table2) => '(#t prime)
; (look-up 20 table3) => '(#t 2)
; (look-up 80 table3) => '(#f #f)

(define look-up
  (lambda (item table)
    (cond [(equal? (first (first table)) item) (list #t (first (rest (first table))))]
          [(empty? (rest table)) (list #f #f)]
          [else (look-up item (rest table))])))


; ** problem 2 ** (10 points)
; Write a predicate (repeat-key? table)
; that takes a table and returns #t if there are two entries
; in the table have the same key

; Examples
; (repeat-key? '()) => #f
; (repeat-key? table1) => #f
; (repeat-key? table2) => #t
; (repeat-key? table3) => #t

(define repeat-key?
  (lambda (table)
    (cond [(empty? table) #f]
          [(empty? (rest table)) #f]
          [(equal? (first (look-up (first (first table)) (rest table))) #t) #t]
          [else (repeat-key? (rest table))])))


; ** problem 3 ** (10 points)
; Write 3 procedures (bnot x), (bor x y), (band x y)
; that compute Boolean not, or, and
; Examples
; (bnot 0) => 1
; (bnot 1) => 0
; (bor 0 1) => 1
; (bor 1 1) => 1
; (band 0 1) => 0
; (band 1 1) => 1

(define bor
  (lambda (x y)
    (cond [(or (= x 1) (= y 1)) 1]
          [else 0])))

(define band
  (lambda (x y)
    (cond [(and (= x 1) (= y 1)) 1]
          [else 0])))

(define bnot
  (lambda (x)
    (cond [(= x 0) 1]
          [else 0])))

; We define a representation of Boolean expressions as follows.
; 0 and 1 represent the constants 0 and 1
; Scheme symbols represent variables
; the list (- exp) represents the Boolean "not" of exp
; the list (+ exp1 exp2) represents the Boolean "or" of exp1 and exp2
; the list (* exp1 exp2) represents the Boolean "and" of exp1 and exp2

; Some examples of Boolean expressions:

(define ex0 '(- x))
(define ex1 '(+ x y))
(define ex2 '(* x y))
(define ex3 '(* x (+ y z)))
(define ex4 '(+ x (- x)))
(define ex5 '(* (+ x 0) (+ x 1)))


; ** problem 4 ** (10 points)
; Write a procedure (get-type exp)
; that takes a Boolean expression as defined above
; and returns its type as one of the symbols:
;   constant, variable, not, or, and
; Note that the type is determined by the top-level
; operation in case the expression is not a constant or variable.
; You may use the Racket predicates number? and symbol?

; Examples
; (get-type 0) => 'constant
; (get-type 'x) => 'variable
; (get-type ex0) => 'not
; (get-type ex1) => 'or
; (get-type ex2) => 'and
; (get-type ex3) => 'and
; (get-type ex4) => 'or


(define get-type
  (lambda (exp)
    (cond [(number? exp) 'constant]
          [(and (not (list? exp)) (not (or (equal? exp '-) (equal? exp '+) (equal? exp '*))) (symbol? exp)) 'variable]
          [(and (not (list? exp)) (equal? exp '-)) 'not]
          [(and (not (list? exp)) (equal? exp '+)) 'or]
          [(and (not (list? exp)) (equal? exp '*)) 'and]
          [(equal? (first exp) '-) 'not]
          [(equal? (first exp) '+) 'or]
          [(equal? (first exp) '*) 'and])))


; We define an environment as a table containing entries
; whose keys are variables and values are Boolean constants
; For example

(define environ1 '((x 0) (y 1)))
(define environ2 '((x 1) (y 0) (z 1)))

; ** problem 5 ** (10 points)
; Write a procedure (eval-in-env exp env)
; that takes a Boolean expression exp
; and an environment env
; represented as described above
; and returns 0 or 1 giving the value of the expression in the
; environment
; Hint: use the recursive structure of Boolean expressions

; Examples
; (eval-in-env 1 environ1) => 1
; (eval-in-env 'x environ1) => 0
; (eval-in-env 'x environ2) => 1
; (eval-in-env ex1 environ1) => 1
; (eval-in-env ex2 environ2) => 0
; (eval-in-env ex1 environ2) => 1
; (eval-in-env ex3 environ2) => 1
; (eval-in-env '(* 1 0) '()) => 0

(define eval-in-env
  (lambda (exp env)
    (cond [(not (list? exp)) (if (equal? (get-type exp) 'constant) exp (first (rest (look-up exp env))))]
          [(list? (first exp)) (eval-in-env (first exp) env)]
          [(empty? (rest exp)) (if (equal? (get-type  exp) 'constant) (first exp) (first (rest (look-up (first exp) env))))]
          [(equal? (get-type exp) 'or) (bor (eval-in-env (first (rest exp)) env) (eval-in-env (rest (rest exp)) env))]
          [(equal? (get-type exp) 'and) (band (eval-in-env (first (rest exp)) env) (eval-in-env (rest (rest exp)) env))]
          [(equal? (get-type exp) 'not) (bnot (eval-in-env (first (rest exp)) env))])))



; ** problem 6 ** (10 points)
; Write the procedure (all-lists n)
; that takes a non-negative integer n
; and creates the list of all 
; lists of n 0's or 1's in the
; *specific order* required for
; a truth table.

; In other words, the lists, interpreted
; as binary numbers, should be in increasing
; order.

; For example
; (all-lists 0) => '(())
; (all-lists 1) => '((0) (1))
; (all-lists 2) => '((0 0) (0 1) (1 0) (1 1))
; (all-lists 3) => '((0 0 0) (0 0 1) (0 1 0) (0 1 1)
;                   (1 0 0) (1 0 1) (1 1 0) (1 1 1)) v


(define all-lists
  (lambda (n)
    (if (= n 0)
        '(())
        (reverse (fix-last (reverse (flat-list (aux-list (- n 1) '(0)) (aux-list (- n 1) '(1)))) '())))))

; fix-last is my super sketchy function which solves a problem in my flat-list function. The last series of numbers were just being cons'd at the end of the list, so fix-last sticks em back on.
(define fix-last
  (lambda (lst fixed)
    (if (not (list? (first lst)))
        (fix-last (rest lst) (cons (first lst) fixed))
        (cons fixed lst))))

; aux-list handles the legwork and does the actual branching and cons-ing onto its own calls.
(define aux-list
  (lambda (n lst)
    (if (= n 0) 
        lst
        (flat-list (aux-list (- n 1) (reverse (cons 0 (reverse lst)))) (aux-list (- n 1) (reverse (cons 1 (reverse lst))))))))
; flat-list is my replacement for cons that takes a flat approach to the list instead of nesting via depth, which was my original problem with the "tree" implementation (one i did not have 
; when I was considering an "add-and-carry" implementation). It has a problem solved by fix-last.
(define flat-list
  (lambda (lst1 lst2)
    (cond [(empty? (rest lst2)) (cons lst1 lst2)]
          [(not (list? (first lst1))) (cons lst1 lst2)]
          [(not (list? (first lst2))) (cons lst1 lst2)]
          [(not (empty? lst1)) (cons (first lst1) (flat-list (rest lst1) lst2))]
          [(not (empty? lst2)) (cons (first lst2) (flat-list lst1 (rest lst2)))]
          [else '()])))
     
; ** problem 7 ** (10 points)

; Create a procedure all-combs that
; takes a list of variables and
; generates all environments  assigning Boolean values to
; all the variables on the list
; example: (all-combs '(x y)) => 
; '(((x 0) (y 0)) ((x 0) (y 1)) ((x 1) (y 0)) ((x 1) (y 1)))



(define all-combs
  (lambda (vars)
    (comb-aux vars (all-lists (length vars)))))

(define (comb-aux vars environs)
  (if (empty? (rest environs))
      (cons (unpack vars (first environs)) '())
      (cons (unpack vars (first environs)) (comb-aux vars (rest environs)))))

(define (unpack vars set)
  (if (empty? (rest vars))
      (cons (list (first vars) (first set)) '())
      (cons (list (first vars) (first set)) (unpack (rest vars) (rest set)))))


; ** problem 8 ** (10 points)
; Write a procedure (all-variables exp) that takes a Boolean
; expression exp and makes a list containing all the variables
; that occur in exp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in exp (scanning left to right.)
; You may use the Racket procedure remove-duplicates.

; Examples
; (all-variables '(* x (+ (- y) x))) => '(x y)
; (all-variables '(+ 0 1)) => '()
; (all-variables '(+ (+ x (- x)) (* x (- x)))) => '(x)
; (all-variables '(+ (+ z y) (+ x y))) => '(z y x)


(define all-variables
  (lambda (exp)
    (remove-duplicates (fix-vars (aux-vars exp)))))

; fix-vars essentially 'flattens' the depths in the list produced by aux-vars, which has issues that I couldn't fix.
(define fix-vars
  (lambda (lst)
    (if (empty? lst) 
        '()
        (if (list? lst)
            (append (fix-vars (first lst)) (fix-vars (rest lst)))
            (list lst)))))

; aux-vars does the legwork, acting recursively and making the list. It has a problem where items at any depth that aren't the last item get placed into a depth of two, as opposed to the one we want.
; fix-vars fixes this.
(define aux-vars
  (lambda (exp)
    (cond [(empty? exp) '()]
          [(and (empty? (rest exp)) (list? (first exp))) (aux-vars (first exp))]
          [(and (empty? (rest exp)) (equal? (get-type (first exp)) 'variable)) first exp]
          [(list? (first exp)) (cons (aux-vars (first exp)) (aux-vars (rest exp)))]
          [(equal? (get-type (first exp)) 'variable) (cons (first exp) (aux-vars (rest exp)))]
          [else (aux-vars (rest exp))])))


; ** problem 9 ** (10 points)
; Write a procedure (satisfiable exp)
; that tests whether the Boolean expression exp is satisfiable
; or not.  Recall that exp is satisfiable if 
; there exists an environment env assigning 
; a Boolean value to each of its variables such that 
; (eval-in-env exp env) is equal to 1.
; If exp is not satisfiable, your procedure should return #f.
; If exp is satisfiable, your procedure should return some environment
; env such that (eval-in-env exp env) is equal to 1.


; Examples
; (satisfiable 0) => #f
; (satisfiable 1) => '()
; (satisfiable ex0) => '((x 0))
; (satisfiable ex1) => '((x 0) (y 1))
; (satisfiable ex2) => '((x 1) (y 1))
; (satisfiable ex3) => '((x 1) (y 0) (z 1))
; (satisfiable ex4) => '((x 0))
; (satisfiable '(* x (- x))) => #f

; Your procedure may find a different environment,
; as long as it causes the expression to evaluate to 1.

(define satisfiable
  (lambda (exp)
    (if (equal? (get-type exp) 'constant)
        (if (= exp 0)
            #f
            '())
        (aux-sat exp (all-combs (all-variables exp))))))

;aux-sat actually recursively goes through the list of environments, checking each to see if it works and then continuing. 
;I can run the large-exp below, but only once I've increased my allotted Racket memory to 512 MB. My code is NOT particularly efficient, so it will take somewhere around 60~90 seconds.
(define (aux-sat exp list-env)
  (if (empty? (rest list-env))
      (if (list? (first list-env))
          (if (= (eval-in-env exp (first list-env)) 1)
              list-env
              #f)
          (if (= (eval-in-env exp list-env) 1)
              list-env
              #f))
      (if (= (eval-in-env exp (first list-env)) 1)
          (first list-env)
          (aux-sat exp (rest list-env)))))


; Here's a very large expression that may run a long time on your machine
; You might not have enough memory to run it on your lap top -- you may need
; to try it in the Zoo


(define large-exp
 '(* x19 (* x18 (* x17 (* x16 (* x15 (* x14 (* x13 (* x12 (* x11 (* x10 
 (* x9 (* x8 (* x7 (* x6 (* x5 (* x4 (* x3 (+ x2 x1)))))))))))))))))))


; (satisfiable large-exp)
; '( (x19 1) (x18 1) (x17 1) (x16 1) (x15 1) (x14 1) (x13 1) (x12 1) 
; (x11 1) (x10 1) (x9 1) (x8 1) (x7 1) (x6 1) (x5 1) (x4 1) 
; (x3 1) (x2 0) (x1 1))

; ** problem 10 ** (10 points)

; The truth table for a Boolean bnew is as follows
;_________
; x  y  bnew
; 0  0  1
; 0  1  1
; 1  0  1
; 1  1  0
;
;
; Let (! exp1 exp2) represent the Boolean bnew of exp1 and exp2.
; Write a procedure convert-bnew to take any expression
; written with *, + and - and convert it to an
; equivalent expression with ! only

; Examples
; (convert-bnew ex0) => '(! x x)
; (convert-bnew ex1) => '(! (! x x) (! y y))
; (convert-bnew ex2) => '(! (! x y) (! x y))
; (convert-bnew ex3) => '(! (! x (! (! y y) (! z z))) (! x (! (! y y) (! z z))))
; (convert-bnew ex4) => '(! (! x x) (! (! x x) (! x x)))
;
; Note from these examples, the expressions do not have to be the simplest form possible
; Your result is correct as long as it is equivalent to the input and only uses !, that is, it does
; not have to be precisely the same as the given examples.
  

(define convert-bnew
  (lambda (exp)
    (cond [(equal? (get-type (first exp)) 'and) (and-con (first (rest exp)) (first (rest (rest exp))))]
          [(equal? (get-type (first exp)) 'or) (or-con (first (rest exp)) (first (rest (rest exp))))]
          [(equal? (get-type (first exp)) 'not) (not-con (first (rest exp)))])))
; and-con handles the syntax for and statements in 'bnew logic': (! (! x y) (! x y)), as well as passing back to convert-bnew if either of its arguments are lists.
(define and-con
  (lambda (exp1 exp2)
    (cond [(and (list? exp1) (list? exp2)) (list '! (list '! (convert-bnew exp1) (convert-bnew exp2)) (list '! (convert-bnew exp1) (convert-bnew exp2)))]
          [(list? exp1) (list '! (list '! (convert-bnew exp1) exp2) (list '! (convert-bnew exp1) exp2))]
          [(list? exp2) (list '! (list '! exp1 (convert-bnew exp2)) (list '! exp1 (convert-bnew exp2)))]
          [else (list '! (list '! exp1 exp2) (list '! exp1 exp2))])))
; or-con does much the same as and-con, but uses different syntax: (! (! x x) (! y y))
(define or-con
  (lambda (exp1 exp2)
    (cond [(and (list? exp1) (list? exp2)) (list '! (list '! (convert-bnew exp1) (convert-bnew exp1)) (list '! (convert-bnew exp2) (convert-bnew exp2)))]
          [(list? exp1) (list '! (list '! (convert-bnew exp1) (convert-bnew exp1)) (list '! exp2 exp2))]
          [(list? exp2) (list '! (list '! exp1 exp1) (list '! (convert-bnew exp2) (convert-bnew exp2)))]
          [else (list '! (list '! exp1 exp1) (list '! exp2 exp2))])))

; not-con acts like the others, but of course only takes one argument. (! x x)
(define not-con
  (lambda (exp1)
    (cond [(list? exp1) (list '! (convert-bnew exp1) (convert-bnew exp1))]
          [else (list '! exp1 exp1)])))

