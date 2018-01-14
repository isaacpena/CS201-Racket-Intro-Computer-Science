#lang racket
(provide
 isort
 insert
 msort
 merge
 qsort
 key<=?
 random-list
 lookup
 depth
 update
 update-table-tree
 binary-table-tree
 lookup-table-tree
 )

; Name: Isaac Pena
; Email address: isaac.pena@yale.edu

; CS 201a HW #7  DUE 11:59 pm Wednesday, Dec 10, 2014
; using the submit command.  
; NO LATE ASSIGNMENTS ACCEPTED
; NO LATE ASSIGNMENTS ACCEPTED
; NO LATE ASSIGNMENTS ACCEPTED
; NO LATE ASSIGNMENTS ACCEPTED

; Here is a library  and some code that will allow you to
; time the execution of procedures.


; include library and procedure definitions for timings
(#%require srfi/19)

(define make-timer
  (lambda ()
    (let ((start-time (current-time)))
      (lambda ()
	(let ((elapsed (time-difference (current-time) start-time)))
	  (+ (exact->inexact (time-second elapsed))
	     (/ (exact->inexact (time-nanosecond elapsed))
		(* 1.0e3 (time-resolution)))))))))
; example of using make-timer to
; create a procedure for timing a sort of time testsort

(define time-sorter
  (lambda (sorter lst compare?)
    (let ((t1 (make-timer)))
      (sorter lst compare?)
      (t1))))



; Unless the problem specifies otherwise:
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.
; * Your procedures need *not* fail gracefully if their
; inputs do not satisfy the assumptions of the problem.
; * Please use the specified names (and numbers of arguments)
; for the required procedures in a problem; this is to
; facilitate automatic testing.
; * Do NOT use MUTATORS (ie. set!, set-car! or set-cdr!)
; * Do NOT use "define" in the body of a procedure definition
; Use the procedures we have been using in class, in other
; assignments, and that are introduced here.


; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 10)

; ************************************************************************
; ** problem 1 ** (16 points)
; Write two procedures for insertion sort:
; (insert item lst compare?)
; (isort lst compare?)
; where lst is a list and compare? is a procedure to
; compare two list elements and return either #t or #f.
; Insertion sort was described in lecture.


; (insert item lst compare?) returns a list equal to lst
; with item inserted in the correct order according to compare?,
; assuming that lst is already correctly ordered by compare?
; (isort lst compare?) insertion sorts lst using insert and
; compare? and returns the correctly sorted list.

; Examples:
; (insert "alonzo" '("algorithm" "program") string<=?) => '("algorithm" "alonzo" "program")
; (insert 7 '(2 3 5 11 13 17) <=) => '(2 3 5 7 11 13 17)
; (isort '(1 3 5 7 2 4 6 8) >=) => '(8 7 6 5 4 3 2 1)
; (isort '(1 3 5 7 2 4 6 8) <=) => '(1 2 3 4 5 6 7 8)
; ************************************************************************

; insertion sort with comparison predicate compare?

(define isort
  (lambda (lst compare?) 
    (isort-aux (rest lst) (insert (first lst) '() compare?) compare?)))

(define isort-aux
  (lambda (lst built compare?)
    (if (null? lst)
        built
        (isort-aux (rest lst) (insert (first lst) built compare?) compare?))))

; insert item into sorted list lst
; with respect to the comparison predicate compare?

(define insert
  (lambda (item lst compare?)
    (if (null? lst)
        (cons item '())
        (if (compare? item (first lst))
            (cons item (cons (first lst) (rest lst)))
            (cons (first lst) (insert item (rest lst) compare?))))))
        
        

; ************************************************************************
; ** problem 2 ** (16 points)
; Write two procedures for merge sort:
; (merge lst1 lst2 compare?)
; (msort lst compare?)
; where lst, lst1 and lst2 are lists and
; compare? is a procedure that compares two list
; elements and returns either #t or #f.
; Merge sort was described in lecture.

; (merge lst1 lst2 compare?)
; returns a list containing all the elements of lst1 and lst2
; correctly ordered according to compare?,
; assuming that lst1 and lst2 are each correctly ordered according
; to compare?
; (msort lst compare?) merge sorts lst using merge and compare? and
; returns the correctly sorted list.

; Examples:
; (merge '(1 14 55 124) '(2 3 150 155) <=) => '(1 2 3 14 55 124 150 155)
; (merge '(124 55 14 1) '(155 150 3 2) >=) => '(155 150 124 55 14 3 2 1)
; (merge '("that" "is" "best") '("where" "were" "they") string>=?) => '("where" "were" "they" "that" "is" "best")
; (merge '("what" "is" "correct") '("where" "was" "it") string>=?) => '("where" "what" "was" "it" "is" "correct")
; (msort '(14 15 2 99 33 100 16) >=) => '(100 99 33 16 15 14 2)
; (msort '(14 15 2 99 33 100 16) <=) => '(2 14 15 16 33 99 100)
; (msort '("is" "best" "where" "they" "that" "were") string<=?) => '("best" "is" "that" "they" "were" "where")
; ************************************************************************

; Merge sort with comparison predicate compare?

(define msort
  (lambda (lst compare?)
    (if (= 1 (length lst))
        lst
        (merge (msort (firsthalf lst (floor (/ (length lst) 2))) compare?) (msort (secondhalf lst (floor (/ (length lst) 2))) compare?) compare?))))

(define firsthalf
  (lambda (lst n)
    (if (= n 0)
        '()
        (cons (first lst) (firsthalf (rest lst) (- n 1))))))

(define secondhalf
  (lambda (lst n)
    (if (= n 0)
        lst
        (secondhalf (rest lst) (- n 1)))))
    


; merges two sorted lists into order
; using comparison predicate compare?

(define merge
  (lambda (lst1 lst2 compare?)
    (cond [(null? lst1) lst2]
          [(null? lst2) lst1]
          [(compare? (first lst1) (first lst2)) (cons (first lst1) (merge (rest lst1) lst2 compare?))]
          [else (cons (first lst2) (merge lst1 (rest lst2) compare?))])))

; ************************************************************
; *** problem 3 ** 12 pts
; *************************************

; In this problem you need to write a procedure
; (qsort lst comp?) that uses the quicksort algorithm to sort a list using
;  the predicate comp? for comparision
; and
; (key<=? lst1 lst2) that is a predicate that compares the key values in 
; two lists '(key1 value1) '(key2 value2), where key1 and key2 are integers.

; Here is a procedure for quicksort,slightly modified from 
; http://blog.matthewrathbone.com/post/931992362/operation-algorithm-quicksort-in-c-and-scheme
;
(define pHelper (lambda (all chk l m)
                  (cond ((null? all) (cons l (cons chk (cons m '()))))
                        (else
                        (let ((x (first all)))
                          (if (<= x chk) 
                              (pHelper (rest all) chk (cons x l) m)
                              (pHelper (rest all) chk l (cons x m))))))))

(define partition (lambda (l)
                      (pHelper (rest l) (first l) '() '())))



(define quicksort (lambda (l)
                    (cond ((null? l) l)
                          (else
                          (let ((lx (partition l)))
                            (append (quicksort (first lx)) (cons (first (rest lx)) (quicksort (first (rest (rest lx)))))))))))

;
; modify the code for quicksort to write the procedure qsort that takes
; a comparison function comp? as a parameter



(define qsort (lambda (l comp?)
                    (cond ((null? l) l)
                          (else
                           (let ((lx (qpartition l comp?)))
                             (append (qsort (first lx) comp?) (cons (first (rest lx)) (qsort (first (rest (rest lx))) comp?))))))))

(define qpartition (lambda (l comp?)
                      (qpHelper (rest l) (first l) '() '() comp?)))

(define qpHelper (lambda (all chk l m comp?)
                  (cond ((null? all) (cons l (cons chk (cons m '()))))
                        (else
                        (let ((x (first all)))
                          (if (comp? x chk) 
                              (qpHelper (rest all) chk (cons x l) m comp?)
                              (qpHelper (rest all) chk l (cons x m) comp?)))))))


; (qsort '("is" "best" "where" "they" "that" "were") string<=?) => '("best" "is" "that" "they" "were" "where")
; (qsort '(9 21 43 6 7 435) >=) => '(435 43 21 9 7 6)
;
; Consider sorting a table of values given by a list of two items (key value) 
; to put the list in order of ascending key value. The keys will all be integers.
; Write a procedure (key<=? lst1 lst2) that will produce the following:
;
; (qsort '((234 "no") (3 "aaa") (324 "foo") (9 "cat") (5 "dog")) key<=?)
;    => '((3 "aaa") (5 "dog") (9 "cat") (234 "no") (324 "foo"))
; 
; (qsort '((234 234) (37 2345) (23 99900) (1 444) (3 2466) (9 2341)) key<=?) =>
;   '((1 444) (3 2466) (9 2341) (23 99900) (37 2345) (234 234))

(define key<=?
  (lambda (lst1 lst2)
     (if (<= (first lst1) (first lst2))
         #t
         #f)))

; ************************************************************************


; ************************************************************************
; ** problem 4 ** (11 points)

; ************************************************************************

; Create a procedure (random-list n) to generate a list of n random numbers, each of which ranges from [0,n-1]

(define random-list
  (lambda (n)
    (rl-aux n n)))

(define rl-aux
  (lambda (n x)
    (if (= n 0)
        '()
        (cons (random x) (rl-aux (- n 1) x)))))
; sample results, since random numbers are generated, your results will vary

;(random-list 10) => '(7 2 1 7 0 3 8 7 2 8)
;(random-list 10) => '(4 0 7 4 5 4 2 7 1 6)
;(random-list 20  => '(7 11 5 19 17 4 15 16 7 15 12 4 3 6 19 3 10 9 18 9)

; ************************************************************************
; ** problem 5 ** (11 points)
; **
; **
; Use the procedure random-list to create some test lists of integers.
; Use these test lists to create a table comparing timings for isort, msort and
; qsort. Test whether the comparison operator (i.e. <=, <, >= or > ) makes
; difference in your results. 
; For your results show:
; Your table
; a conclusion about the relative efficiency of the sort methods
; a conclusion about whether the comparison operator mattered
;


; Isaac's Table:
; Sample-test at 5000
; Insertion sort with <= : 1.939
; Merge sort with <= : .047 (ocasionally about .031)
; Quicksort with <= : .016 (alternating with about .032)
; Insertion sort with >= : 2.344
; Merge sort with >= : .032 (more often about .046)
; Quicksort with >= : .031 (alternating with about .016)
; Insertion sort with < : 1.892
; Merge sort with < : .047 (ocasionally about .031)
; Quicksort with < : .032 (alternating with about .015)
; Insertion sort with > : 1.953
; Merge sort with > : .046 (ocasionally about .031)
; Quicksort with > : .016 (alternating with about .031)
;
; Sample-test at 20000
; Insertion sort with <= : 34.854
; Merge sort with <= : .172 (ocasionally about .25)
; Quicksort with <= : .093 (alternating with about .156)
; Insertion sort with >= : 40.867
; Merge sort with >= : .172 (ocasionally about .234)
; Quicksort with >= : .093 (alternating with about .14)
; Insertion sort with < : 36.008
; Merge sort with < : .172 (ocasionally about .235)
; Quicksort with < : .094 (alternating with about .141)
; Insertion sort with > : 35.352
; Merge sort with > : .173 (ocasionally about .219)
; Quicksort with > : .093 (alternating with about .157)
;
; Conclusions:
; Insertion sort is immensely worse than quicksort or mergesort. Mergesort seems to be one "step" below quicksort in terms of efficiency, coming up with values around either .172 or .235 for 
; the list of 20000 items while quicksort generates around .094 or .15. As the list length extends, insertion sort falls further behind quick and merge sort, but the discrepancy between quick
; and merge doesn't grow substantially larger (although it does grow slightly and this would be more pronounced at lengths at much higher orders of magnitudes.) 
; As for differences between comparison operators, merge and quick sort don't particularly care, but it matters greatly for insertion sort, which has an entirely different O(n) notation on
; already-sorted lists. If a list is randomly generated but happens to have a general pattern towards least-to-greatest, this is easier for insertion sort to sort in <= than in >=. The equality
; operator applies to that analysis as well, of course. 
;
;
; some examples with times from my Mac: 
; (define sample-test (random-list 5000))
; (time-sorter isort sample-test <=) => 1.8
; (time-sorter msort sample-test <=) => 0.017
; (time-sorter qsort sample-test <=) => .018
; (time-sorter isort sample-test >=) =>  1.7
; (time-sorter msort sample-test >=) =>  .018
; (time-sorter qsort sample-test >=) => .022

; (define sample2-test (random-list 50000))
; (time-sorter qsort sample-test <=) => .261

; some examples with times from a more powerful machine
; (define another-test (random-list 100000))
; (time-sorter msort another-test >=) => 0.217
; (time-sorter isort another-test >=) => 209.125
; (time-sorter qsort another-test >=) => 0.172
    



; ************************************************************************
; ** problem 6 ** (11 points)
; We define a Racket data structure for binary search trees
; using lists as follows.
; In this problem and problem 7, we will consider lists of integers only.
; The empty binary search tree is the empty list '()
; A nonempty binary search tree is a list consisting of
; a number (the root), a binary search tree (the left subtree)
; and another binary search tree (the right subtree).
; All the numbers appearing in the left subtree are less than
; the root, and the root is less than all the numbers appearing
; in the right subtree.
;


; For example, the following binary search tree can be drawn
;              7
;            /   \
;           5     9

(define tree1 '(7 (5 () ()) (9 () ())))

; We define convenient selectors for a binary search tree.

(define root car)
(define left-subtree cadr)
(define right-subtree caddr)

; Write a procedure
; (lookup n tree)
; to look up a number n in the given binary search tree.
; It should return #t if found, #f if not found.
; Please use the selectors defined above.

; Examples
; (lookup 7 tree1) => #t
; (lookup 6 tree1) => #f
; (lookup 5 tree1) => #t
; (lookup 9 tree1) => #t
; (lookup 11 tree1) => #f

; ************************************************************************

(define lookup
  (lambda (n tree)
    (if (null? tree)
        #f
        (if (= (root tree) n)
            #t
            (or (lookup n (left-subtree tree)) (lookup n (right-subtree tree)))))))

; ************************************************************************
; ** problem 7 ** (11 points)
; Write two procedures
; (depth tree)
; (update tree n)
; where n is a number and tree is a binary search tree

; (depth tree) should return the maximum number of comparisons
; required to look up a number in tree -- that is, the length
; of the longest path in tree.

; (update tree n) should return a binary search tree equal to tree
; with n inserted in the correct position.  You may assume that
; n is not already in the tree.

; Examples
; (update tree1 6) => '(7 (5 () (6 () ())) (9 () ()))
; (update (update tree1 8) 11) => '(7 (5 () ()) (9 (8 () ()) (11 () ())))
; (depth tree1) => 2
; (depth '()) => 0
; (depth (update tree1 6)) => 3


; ************************************************************************

(define depth
  (lambda (tree)
    (if (null? tree)
        0
        (depth-aux tree 0))))

(define depth-aux
  (lambda (tree n)
    (if (null? tree)
        n
        (let ((ldep (depth-aux (left-subtree tree) (+ n 1)))
              (rdep (depth-aux (right-subtree tree) (+ n 1))))
          (if (> ldep rdep)
              ldep
              rdep)))))
          

(define update
  (lambda (tree n)
    (if (null? tree)
        (list n '() '())
        (if (> (root tree) n)
            (list (root tree) (update (left-subtree tree) n) (right-subtree tree))
            (list (root tree) (left-subtree tree) (update (right-subtree tree) n))))))

; ************************************************************************
; ** problem 8 ** (11 points)
; 
; We expand the use of binary trees to looking up values in tables.
; Write a procedure (binary-table-tree table compare-key?) that takes a list of table look-up
; values  and produces a binary tree of the values by recursively call a procedure 
; (update-table-tree table-tree table-item compare-key?) where 
; (compare-key? key1 key2) returns #t if the table-item with key1 should appear
; before the item with key2 in the sorted list. 
;
; Write a procedure (lookup-table-tree key tree compare-key?) that returns the value 
; associated with key in the table if it exists, and returns #f otherwise
;

(define update-table-tree
  (lambda (tree item compare-key?)
    (if (null? tree)
        (list item '() '())
        (if (compare-key? (first item) (first (root tree)))
            (list (root tree) (update-table-tree (left-subtree tree) item compare-key?) (right-subtree tree))
            (list (root tree) (left-subtree tree) (update-table-tree (right-subtree tree) item compare-key?))))))


;(update-table-tree '((7 "y") () ()) '(5 "z") <=) => '((7 "y") ((5 "z") () ()) ())
;
;(update-table-tree  '(("icelandic" "íkorni") (("dutch" "eekhoorn") ()()) ()) '("finnish" "orava") string<=?) =>
;'(("icelandic" "íkorni") (("dutch" "eekhoorn") () (("finnish" "orava") () ())) ())


(define binary-table-tree
  (lambda (table compare-key?)
    (let ((unnecessarily-reversed-table (reverse table)))
      (btt-aux (rest unnecessarily-reversed-table) compare-key? (update-table-tree '() (first unnecessarily-reversed-table) compare-key?)))))

(define btt-aux
  (lambda (table compare-key? tree)
    (if (null? table)
        tree
        (btt-aux (rest table) compare-key? (update-table-tree tree (first table) compare-key?)))))

(define test-table '((9 "a") (3 "b") (5 "c") (4 "q") (1 "z") (7 "y") ))
(define translate-table '(("bulgarian" " kateritsa") ("estonian" "orav") ("finnish" "orava")("french" "ecureuil") ("welsh" "gwiwer") ("german" "eichhoernchen") ("italian" "scoiattolo") ("lithuanian" "vovere") ("portuguese" "esquilo") ("romanian" "veverita") ("slovak" "vevericka") ("swedish" "ekorre") ("polish" "wiewiorka") ("dutch" "eekhoorn") ("norwegian" "ekorn") ("irish" "iora rua") ("icelandic" "íkorni")))

;(binary-table-tree test-table <=) -> '((7 "y") ((1 "z") () ((4 "q") ((3 "b") () ()) ((5 "c") () ()))) ((9 "a") () ()))
;(binary-table-tree translate-table string<=?) -> '(("icelandic" "íkorni") (("dutch" "eekhoorn")  (("bulgarian" " kateritsa") () ())  (("german" "eichhoernchen")   (("french" "ecureuil") (("finnish" "orava") (("estonian" "orav") () ()) ()) ())   ())) (("irish" "iora rua")  ()  (("norwegian" "ekorn")   (("lithuanian" "vovere") (("italian" "scoiattolo") () ()) ())   (("polish" "wiewiorka")    ()    (("swedish" "ekorre")     (("slovak" "vevericka") (("romanian" "veverita") (("portuguese" "esquilo") () ()) ()) ())     (("welsh" "gwiwer") () ()))))))


(define lookup-table-tree
  (lambda (key tree compare-key?)
    (if (null? tree)
        #f
        (if (equal? key (first (root tree)))
            (first (rest (root tree)))
            (if (compare-key? key (first (root tree)))
                (lookup-table-tree key (left-subtree tree) compare-key?)
                (lookup-table-tree key (right-subtree tree) compare-key?))))))
            

(define test-tree (binary-table-tree test-table <=))
;(lookup-table-tree 9 test-tree <=) => "a"
; (lookup-table-tree 15 test-tree <=) => #f

(define test-translation (binary-table-tree translate-table string<=?))
;(lookup-table-tree "welsh" test-translation string<=?) => "gwiwer"
;(lookup-table-tree "romanian" test-translation string<=?)=>"veverita"
;(lookup-table-tree "english" test-translation string<=?) => #f

; ************************************************************************


; ************************************************************************
; *** END OF HW #7; END OF HWs; END OF CPSC 201a, 2014!!!!!!!!!***********
; ************************************************************************
