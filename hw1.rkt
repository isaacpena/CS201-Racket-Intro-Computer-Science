#lang racket

; This file may be loaded into DrRacket.
; Lines beginning with semicolons are comments.

; Name: Isaac Pena
; Email address: isaac.pena@yale.edu

; CS 201a HW #1  DUE ** Wednesday Sept 17, 2014 **
; Solutions on paper due in class on Sept 17
; Solutions that procedures due with "Submit" by 11:59pm, Sept 17

; If you are asked to write a procedure, you may write
; auxiliary procedures -- for each of your auxiliary
; procedures, please include a comment explaining what it
; does.

; On inputs other than those specified, your procedures need
; not do anything reasonable.

; Reading: Chapters 1 and 2 of the Racket Guide


; ** problem 0 ** (1 easy point)

; Replace the 0 below with the time you spent on this
; assignment, including reading. Round to the nearest hour.

(define hours 12)


; ** problem 1 ** (11 points)

; Write a procedure (day->name n) that takes as argument an
; integer n from 1 to 5 and returns a symbol giving the name
; of the n^th day in the work week. For any other number, it
; should return the symbol ?.

; Examples
; (day->name 1) => 'Monday
; (day->name 5) => 'Friday
; (day->name 97) => '?

(define (day->name n)
  (cond [(= n 1) 'Monday]
        [(= n 2) 'Tuesday]
        [(= n 3) 'Wednesday]
        [(= n 4) 'Thursday]
        [(= n 5) 'Friday]
        [else '?]))


; ** problem 2 ** (11 points)

; Write a procedure (one-to-five n) that returns the number
; of digits that are between one and five in the decimal
; representation of the nonnegative integer n.

; Examples
; (one-to-five 0) => 0
; (one-to-five 457) => 2
; (one-to-five 88012) => 2

(define (one-to-five n)
  (if (= n 0)
     0
     (if (and (< (remainder n 10) 6) (> (remainder n 10) 0))
         (+ (one-to-five (quotient n 10)) 1)
         (one-to-five (quotient n 10)))))

; ** problem 3 ** (11 points)
; To be handed in on paper in lecture

; Draw the internal list structure (box-and-pointer diagram)
; corresponding to the value of the following expression.

; (list  (list '(4 a) (list 'b)) (cons 2 3)  1 0 )


; ** problem 4 ** (11 points)
; To be handed in on paper in lecture

; For the following two procedures, describe what each
; computes, and how it does so.

(define puzzle1
  (lambda (x)
    ((if (>= 0 x) + -)
     0
     x)))

(define puzzle2
  (lambda (a b c)
    (let ((a c)
          (b a))
      (+ a b))))


; ** problem 5 ** (11 points)

; Write a procedure (repeat itm lst) that takes an item itm
; and a list lst and returns a list equal to lst with every
; top-level occurrence of itm repeated. Please use the
; equal? procedure to test equality of values.

; Examples
; (repeat 1 '(1 2 3)) => '(1 1 2 3)
; (repeat 1 '((2 1) (4 5) (7 6))) => '((2 1) (4 5) (7 6))
; (repeat 'a '(f a f a b e b a)) => '(f a a f a a  b e b a a)
; (repeat '(2 1) '((3 4) (2 1) (1 2))) => '((3 4) (2 1) (2 1) (1 2))

(define (repeat itm list)
  (if (not (empty? list))
      (if (equal? itm (first list))
          (cons (first list) (cons itm (repeat itm (rest list))))
          (cons (first list) (repeat itm (rest list))))
      list))


; ** problem 6 ** (11 points)

; Write a procedure (prefix? lst1 lst2) that takes two lists
; lst1 and lst2 and returns #t if lst1 can be obtained by
; dropping zero or more elements from the end of lst2.
; Otherwise, #f is returned.

; Use prefix? to write a procedure (sublist? lst1 lst2) that
; returns #t if lst1 is a contiguous top-level sublist of
; lst2, and #f otherwise.

; Examples
; (prefix? '(a b) '(a b c)) => #t
; (prefix? '(b c) '(a b c)) => #f
; (prefix? '() '()) => #t
; (prefix? '() '(1 2 3)) => #t

; (sublist? '(a b) '(a b c)) => #t
; (sublist? '(b c) '(a b c d)) => #t
; (sublist? '(a c) '(a b c d)) => #f
; (sublist? '() '(1 2 3)) => #t
; (sublist? '(a b d) '(a b c a b d e)) => #t

(define (prefix? lst1 lst2)
  (if (not (empty? lst2))
      (if (equal? lst1 lst2)
          #t
          (prefix? lst1 (reverse (rest (reverse lst2)))))
      (if (empty? lst1)
          #t
          #f)))

(define (sublist? lst1 lst2)
  (if (not (empty? lst2))
      (if (prefix? lst1 lst2)
          #t
          (sublist? lst1 (rest lst2)))
      (if (empty? lst1)
          #t
          #f)))

; ** problem 7 (11 points)

; Write a procedure (split lst i j) that takes a list lst
; and nonnegative integers i and j (where the sum of i and j
; are between 0 and the length of lst) and returns a list of
; three lists, consisting of the first i elements of lst,
; the next j elements and the rest of the elements of lst.

; Examples
; (split '(a b a c) 0 0 ) => '(() () (a b a c))
; (split '(a b a c) 2 1) => '((a b) (a) (c))
; (split '(a b a c) 4 0) => '((a b a c) () ())
; (split '(a b a (b a)) 3 1) => '((a b a) ((b a)) ())

; (firstpart) generates the first sublist.
(define (firstpart lst i)
  (if (= i 0)
      '()
      (cons (first lst) (firstpart (rest lst) (- i 1)))))

; (secondpart) generates the second sublist.
(define (secondpart lst j)
  (if (= j 0)
      '()
      (cons (first lst) (secondpart (rest lst) (- j 1)))))

; (thirdpart) generates the remaining, and last, sublist.
(define (thirdpart lst)
  (if (empty? lst)
      '()
      (cons (first lst) (thirdpart (rest lst)))))

; (homebrew-listtail) is my version of list-tail, used to grab the indexes that are input for the previous three procedures.
(define (homebrew-listtail lst ind)
  (if (= ind 0)
      lst
      (homebrew-listtail (rest lst) (- ind 1))))

(define (split lst i j)
  (list (firstpart lst i) (secondpart (homebrew-listtail lst i) j) (thirdpart (homebrew-listtail lst (+ i j)))))
  
        

; ** problem 8 (11 points)

; Write a procedure (delete-exp exp1 exp2) that deletes
; every occurrence of expression exp1 in expression exp2.

; Examples
; (delete-exp 'a '(f a f a b e b a)) => '(f  f  b e b )
; (delete-exp 'a '((f a) (f (a)))) => '((f ) (f ()))
; (delete-exp 'x  '(rest (rest x))) => '(rest (rest))
; (delete-exp 'z   '(f a a b e b)) => '(f a a b e b)
; (delete-exp '(f a) '((f a) ((f a)) (((f a))))) => '(() (()))

; (look-deeper) handles sublists inside the main exp2. 
(define (look-deeper exp1 exp2)
  (if (not (empty? exp2))
      (if (equal? exp1 (first exp2))
          (look-deeper exp1 (rest exp2))
          (if (list? (first exp2))
              (cons (look-deeper exp1 (first exp2)) (rest exp2))
              (cons (first exp2) (look-deeper exp1 (rest exp2)))))
      '()))

(define (delete-exp exp1 exp2)
  (if (not (empty? exp2))
      (if (equal? exp1 (first exp2))
          (delete-exp exp1 (rest exp2))
          (if (list? (first exp2))
              (cons (look-deeper exp1 (first exp2))  (delete-exp exp1 (rest exp2)))
              (cons (first exp2) (delete-exp exp1 (rest exp2)))))
     '()))


; ** problem 9 (11 points)

; Write a procedure (find-in exp1 exp2) that returns (if
; possible) an expression consisting of the procedures cons,
; first, and rest and the variable x, such that if exp2 is
; substituted for x and evaluated, the result is exp1. If
; this is not possible, return #f.

; Describe a method of testing the resulting expression.

; Examples
; (find-in 'a 'a) => 'x
; (find-in 'a 'b) => #f
; (find-in 'a '(b c)) => #f
; (find-in 'a '(a b c)) => '(first x)
; (find-in '(b c) '(a b c)) => '(rest x)
;
; (find-in '(a c) '(a b c)) =>
;   '(cons (first x) (rest (rest x)))
;
; (find-in '(c b) '(a b c)) =>
;   '(cons (first (rest (rest x)))
;          (cons (first (rest x))
;                (rest (rest (rest x)))))
;
; (find-in '(a (a b)) '(a b c d)) =>
;   '(cons (first x)
;          (cons (cons (first x)
;                      (cons (first (rest x))
;                            (rest (rest (rest (rest x))))))
;                (rest (rest (rest (rest x))))))
;
;
; (find-in '() '(a b c)) =>
;   '(rest (rest (rest x)))
;
; (find-in '(a c) '(((b c)) (a a a) (d))) =>
;   '(cons (first (first (rest x)))
;          (rest (first (first x))))

;(rest-to-end) is used to calculate the end of the second expression, returning (rest (rest (rest (rest ... (rest x))) ... )
(define (rest-to-end exp2)
  (if (empty? exp2)
      'x
      (list 'rest (rest-to-end (rest exp2)))))

;(recursive-find) handles stepping through exp2 and building the stack.

(define (recursive-find char exp)
  (cond [(list? (first exp)) (list (recursive-find char (first exp)) (recursive-find char (rest exp)))]
        [(equal? char (first exp)) 'x]
        [(equal? '() (rest exp)) 'a]
        [else (list 'rest (recursive-find char (rest exp)))]))

; Testing the results of (find-in) is as simple as plugging in x to the resulting expression and seeing if it matches exp1.
(define (find-in exp1 exp2)
  (cond [(equal? '() exp2) #f]
        [(empty? exp2) #f]
        [(and (not (list? exp1)) (not (list? exp2)) (not (equal? exp1 exp2))) #f]
        [(and (not (list? exp1)) (not (list? exp2)) (equal? exp1 exp2)) 'x]
        [(equal? '() exp1) (rest-to-end exp2)]
        [(not (list? (first exp1))) (list (list 'first (recursive-find (first exp1) exp2)) (find-in (rest exp1) exp2))]
        [else (list (or (find-in (first exp1) exp2) (find-in (rest exp1) exp2)))]))
          
