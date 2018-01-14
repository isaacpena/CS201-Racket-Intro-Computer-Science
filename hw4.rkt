#lang racket

;
(provide
 hours
 left-lookup
 right-lookup
 update
 updates
 extract 
 fill
 bits->nonneg nonneg->bits 
 bits->tcint tcint->bits add-bits sub-bits
 symbol-table 
label?
assemble
init-config
memory-read
memory-write
next-config 
 )

; Name: Isaac Pena
; Email address: isaac.pena@yale.edu

; CS 201a HW #4  DUE Tuesday, Oct. 21, at 11:59 pm, electronically,
; using the submit command.
; Lateness penalties (5 points per day) will apply.

; Computer science topics: representations of integers, 
; translating and simulating TC-201 instructions.

; Unless the problem specifies otherwise:
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.
; * Your procedures need *not* fail gracefully if their
; inputs do not satisfy the assumptions of the problem.
; * Please use the specified names (and numbers of arguments)
; for the required procedures in a problem; this is to
; facilitate automatic testing.
; * Only use Racket procedures that have been used in class
; or which were allowed in previous homework or which are specified
; in the problem description.

; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment (including reading):

(define hours 1)

; ** problem 1 (9 points)
; Write four procedures: left-lookup, right-lookup
; update and updates, that operate on tables, where
; a table is defined to be a list of (val1 val2)
; entries. We refer to val1 as the left-value and to
; val2 as the right value.

; We can get the first entry of a table using the
; built in Racket procedure first, and the rest of the
; table with the built in procedure rest

; When we have an individual entry we can obtain 
; the left-value using first, and we can obtain
; the right-value by using first on the rest of the entry.
; for entries:


; (left-lookup val table) finds the first entry whose
; left-value is equal to val, and returns the
; right-value of the entry.  Otherwise, it returns #f.

; (right-lookup val table) finds the first entry whose
; right-value is equal to val, and returns
; the left-value of the entry.  Otherwise,
; it returns #f.

; (update new-entry table) finds the first entry
; whose left-value is equal to the first element
; of new-entry, and returns a table in which
; that entry is replaced by new-entry.  If there are no entries
; whose left-value is equal to the left-value
; of new-entry, then it returns a table in which 
; new-entry is added at the end.

; (updates lst table) takes a list lst of new entries
; and returns a table that is the
; result of making all the indicated updates.


; Examples
; (left-lookup 'a '((a 3) (b 2) (c 1))) => 3
; (left-lookup 'b '((a 3) (b 2) (c 1))) => 2
; (left-lookup 'd '((a 3) (b 2) (c 1))) => #f

; (right-lookup 1 '((a 3) (b 2) (c 1))) => c
; (right-lookup 3 '((a 3) (b 3) (c 3))) => a
; (right-lookup 5 '((a 3) (b 2) (c 1))) => #f

; (update '(a 3) '((a 1) (b 2) (c 3))) => '((a 3) (b 2) (c 3))
; (update '(d 4) '((a 1) (b 2) (c 3))) => '((a 1) (b 2) (c 3) (d 4))
; (update '(c 9) '((a 1) (b 4) (c 3))) => '((a 1) (b 4) (c 9))

; (updates '() '((a 1) (b 2) (c 3))) => '((a 1) (b 2) (c 3))
; (updates '((a 1) (b 2) (c 3)) '()) => '((a 1) (b 2) (c 3))
; (updates '((a 3) (c 4) (d 1)) '((a 1) (b 2) (c 3))) =>
;      '((a 3) (b 2) (c 4) (d 1))

(define left-lookup
  (lambda (val table)
    (cond [(empty? table) #f]
          [(equal? val (first (first table))) (first (rest (first table)))]
          [else (left-lookup val (rest table))])))

(define right-lookup
  (lambda (val table)
    (cond [(empty? table) #f]
          [(equal? val (first (rest (first table)))) (first (first table))]
          [else (right-lookup val (rest table))])))

(define update
  (lambda (new-entry table)
    (cond [(empty? table) (cons new-entry '())]
          [(equal? (first new-entry) (first (first table))) (cons new-entry (rest table))]
          [else (cons (first table) (update new-entry (rest table)))])))

(define updates
  (lambda (lst table)
    (if (empty? lst) 
        table
        (updates (rest lst) (update (first lst) table)))))


; ** problem 2 ** (9 points)
; Write two procedures:

; (extract i j lst) 
; that takes a list lst and nonnegative integers i and j
; and returns the list of elements indexed i through j
; As in list-ref, list elements are indexed from 0.

; (fill n lst)
; that takes a nonnegative integer n and a list lst,
; and if n is greater than the length of lst, returns
; a list equal to lst with enough 0's added at the
; beginning to make the length of the list n.
; If n is less than or equal to the length of lst,
; it is returned as is.

; Examples:
; (extract 1 3 '(a b c d e)) => '(b c d)
; (extract 4 4 '(a b c d e)) => '(e)
; (extract 0 0 '(a b c)) => '(a)
; (extract 0 2 '(a b c)) => '(a b c)

; (fill 12 '(1 0 0)) => '(0 0 0 0 0 0 0 0 0 1 0 0)
; (fill 4 '(1 0 0)) => '(0 1 0 0)
; (fill 3 '(1 0 0)) => '(1 0 0)
; (fill 2 '(1 0 0)) => '(1 0 0)
; (fill 3 '()) => '(0 0 0)

(define extract
  (lambda (i j lst)
    (extract-counter i j lst 0)))

(define (extract-counter i j lst count)
  (cond [(< count i) (extract-counter i j (rest lst) (+ 1 count))]
        [(and (>= count i) (< count j)) (cons (first lst) (extract-counter i j (rest lst) (+ 1 count)))]
        [(>= count j) (cons (first lst) '())]))

(define fill
  (lambda (n lst)
    (if (> n (length lst))
        (aux-fill (- n (length lst)) lst)
        lst)))

(define aux-fill 
  (lambda (n 
           lst)
    (if (> n 0)
        (cons 0 (aux-fill (- n 1) lst))
        lst)))


; ** problem 3 ** (9 points)
; Write two procedures 
; (bits->nonneg lst) and (nonneg->bits n)

; (bits->nonneg lst) takes a list of bits lst
; and returns the value of the nonnegative number 
; represented in binary by those digits.
; Note the special case of '() representing 0.

;(nonneg->bits n) takes a nonnegative integer n
; and returns the list of bits representing n in binary.
; Note that for 0 the answer is '(0) but for
; all other numbers the answer starts with 1.

; Examples:
; (bits->nonneg '()) => 0
; (bits->nonneg '(0)) => 0
; (bits->nonneg '(0 1 0)) => 2
; (bits->nonneg '(1 1 0)) => 6
; (bits->nonneg '(1 0 1 1)) => 11
; (bits->nonneg '(0 0 0 1)) => 1

; (nonneg->bits 0) => '(0)
; (nonneg->bits 6) => '(1 1 0)
; (nonneg->bits 14) => '(1 1 1 0)
; (nonneg->bits 7) => '(1 1 1)

(define bits->nonneg
  (lambda (lst)
        (aux-dec (reverse lst) 1)))

(define (aux-dec lst pow)
  (if (empty? lst)
      0
      (+ (* (first lst) pow) (aux-dec (rest lst) (* pow 2)))))


(define nonneg->bits
  (lambda (n)
    (if (= n 0)
        '(0)
        (reverse (aux-bin n)))))

(define (aux-bin n)
  (if (= n 0)
      '()
      (cons (remainder n 2) (aux-bin (quotient n 2)))))


; This term the TC-201 uses sign-magnitude representation
; of integers.
; The next problem asks you to implement this arithmetic.
; Note that in sign-magnitude arithmetic with 16
; bits, we can represent numbers from -32767 to +32767,
; and there are both +0 and -0.

; ** problem 4 ** (9 points)
; Write the following four procedures:
; bits->tcint, tcint->bits, add-bits, sub-bits

; (bits->tcint lst) input is a list of 16 bitsb
; the returned value is an integer (positive, zero, or negative) 
; whose value is represented by the 16 bits (in sign-magnitude.)

; (tcint->bits x) input is an integer x (positive, zero, or negative)
; the returned value is a list of *two items*:
; either the symbol 'ok or 'overflow
; and a list of 16 bits.
; If x can be correctly represented in sign-magnitude
; arithmetic in 16 bits, then the symbol is 'ok and
; the 16 bits give the correct representation.
; If x cannot be correctly represented, then the
; symbol is 'overflow and the 16 bits are all zeros.

; (add-bits lst1 lst2) input is two lists of 16 bits
; the returned value is a list of two items:
; either the symbol 'ok or 'overflow
; and a list of 16 bits.
; If the sum of the numbers represented by lst1 and
; lst2 can be correctly represented in 16 bits  in the TC-201,
; then the symbol is 'ok and the 16 bits is the representation
; of the sum.
; If the sum cannot be correctly represented, then
; the symbol is 'overflow and the 16 bits are all zeros.

; (sub-bits lst1 lst2) input is two lists of 16 bits
; the returned value is a list of two items:
; either the symbol 'ok or 'overflow
; and a list of 16 bits.
; If the value of the difference of the two numbers 
; (first minus second) represented by lst1 and lst2 can
; be correctly represented, then the symbol is 'ok
; and the 16 bits represents the difference.
; If the difference cannot be correctly represented, then
; the symbol is 'overflow and the 16 bits are all zeros.

; (Note that producing the all zeros results in case of
; error might not be the most useful design, but it is
; simple to implement.)

; Here are some constants

(define twotothe16 65536)
(define twotothe15 32768)
(define twotothe12  4096)

; Here are some 16 bit quantities to use for tests

(define zero16   '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
(define mzero16  '(1 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
(define one16    '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1))
(define mone16   '(1 0 0 0  0 0 0 0  0 0 0 0  0 0 0 1))
(define two16    '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 1 0))
(define mtwo16   '(1 0 0 0  0 0 0 0  0 0 0 0  0 0 1 0))
(define large16  '(0 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1))
(define mlarge16 '(1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1))

; Examples:

; (tcint->bits 1) => '(ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
; (tcint->bits 0) => '(ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (tcint->bits -1) => '(ok (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
; (tcint->bits 14) => '(ok (0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0))
; (tcint->bits 32767) => '(ok (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
; (tcint->bits -32767) => '(ok (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
; (tcint->bits 32768) => '(overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (tcint->bits -32768) => '(overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (tcint->bits 65535) => '(overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; (bits->tcint large16) => 32767
; (bits->tcint mone16) =>  -1
; (bits->tcint one16) => 1
; (bits->tcint zero16) => 0
; (bits->tcint mzero16) => 0
; (bits->tcint '(1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0) => -14
; (bits->tcint mlarge16) => -32767

; (add-bits one16 one16) => '(ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
; (add-bits large16 zero16) => '(ok (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
; (add-bits large16 one16) => '(overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (add-bits one16 mone16) => '(ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (add-bits mone16 mone16) => '(ok (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
; (add-bits mlarge16 mone16) => '(overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (add-bits mlarge16 one16) => '(ok (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))

; (sub-bits mlarge16 mlarge16) => '(ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (sub-bits one16 one16) => '(ok (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (sub-bits large16 one16) => '(ok (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
; (sub-bits one16 large16) => '(ok ((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
; (sub-bits mone16 one16) => '(ok (1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
; (sub-bits mlarge16 large16) => '(overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (sub-bits large16 mlarge16) => '(overflow (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))


(define bits->tcint 
  (lambda (lst)
    (if (= (first lst) 0)
        (bits->nonneg (rest lst))
        (- 0 (bits->nonneg (rest lst))))))

(define tcint->bits
  (lambda (x)
    (if (or(> -32767 x) (< 32767 x))
             (list 'overflow (fill 16 '()))
             (list 'ok (cons (if (<= 0 x)
                                 0
                                 1) (fill 15 (nonneg->bits (abs x))))))))
          

(define add-bits
  (lambda (lst1 lst2)
    (tcint->bits (+ (bits->tcint lst1) (bits->tcint lst2)))))

(define sub-bits
  (lambda (lst1 lst2)
    (tcint->bits (- (bits->tcint lst1) (bits->tcint lst2)))))





; An assembly language program for the TC-201 is represented
; as a list of items, each of which represents an instruction or
; a data statement, as follows.

; Each item is a list, which may optionally have a label as its
; first element.  A label is a symbol ending in a colon (:).

; For an instruction with an address (load, store, add, sub,
; input, output, jump, loadi, storei), 
; the first (or next) symbol is the opcode, and the
; final entry is a symbol or a nonnegative number
; representing the contents of the address field.

; For an instruction with no address (halt, skipzero, skippos,
; skiperror), the first (or next) symbol is the opcode,
; and there are no further entries.

; For a data statement, the first (or next) symbol is the symbol
; 'data, followed by an integer (positive, zero, or negative)
; or a symbol, representing the value to be placed 
; in the corresponding memory location.

; Examples of programs:

; Initializes count to 1 and halts.

(define prog1
  '((start: load one)
    (store count)
    (halt)
    (one: data 1)
    (count: data 0)))
;
;  numbers are input and stored in consecutive locations
; starting with table, until a zero is input, then halts.

(define prog2
  '((start: input val)
    (load val)
    (skipzero)
    (jump continue)
    (halt)
    (continue: storei pointer)
    (load pointer)
    (add one)
    (store pointer)
    (jump start)
    (one: data 1)
    (val: data 0)
    (pointer: data table)
    (table: data 0)))


; ** problem 5 ** (9 points)
; Write a procedure (symbol-table prog address)
; that takes a TC-201 program prog and a starting address
; and returns a table containing all the labels
; defined in the program and their corresponding addresses
; when the program is loaded 
; *starting at the given address*.

; The order of the labels in the table should be the
; order in which they are first defined in the program.
; You may assume that no label will be defined more than once.

; Note that a symbol is a label if it is first in the list
; and its name ends with colon (:).  For this assignment
; for the purpose of identifying labels
; you may use the following procedures: symbol->string,
; string-ref, string-length, and the representation of
; the character constant #\:
; The descriptions of these procedures can be found
; in the Racket guide.
; Here is a procedure to determine whether
; a symbol ends with the colon (:)

(define label?
  (lambda (sym)
    (let ((str (symbol->string sym)))
      (let ((char (string-ref str (- (string-length str) 1))))
	(equal? char #\:)))))


; Examples:
; (symbol-table '((here: load 14) (store 15) (there: hlt)) 0) =>
;         '((here: 0) (there: 2))
; (symbol-table prog1 5) =>
;         '((start: 5) (one: 8) (count: 9))
; (symbol-table prog1 40) =>
;         '((start: 40) (one: 43) (count: 44))
; (symbol-table prog2 0) =>
;   '((start: 0) (continue: 5) (one: 10) (val: 11) (pointer: 12) (table: 13))

(define symbol-table
  (lambda (prog address)
    (cond [(empty? prog) '()]
          [(label? (first (first prog))) (cons (list (first (first prog)) address) (symbol-table (rest prog) (+ address 1)))]
          [else (symbol-table (rest prog) (+ address 1))])))
          




; ** problems 6&7 ** (18 points)
; Write a procedure (assemble prog address)
; that takes a symbolic TC-201 program prog and
; a starting address, and returns the list of
; 16-bit values (represented as lists of bits)
; that represent that program 
; when loaded *at that address*.

; Examples (formatted for readability):

; (assemble '((halt)) 3) =>
; '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; (assemble '((data -1)) 0) =>
; '((1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

; (assemble '((load 22) (store 23)) 0) =>
; '((0 0 0 1 0 0 0 0 0 0 0 1 0 1 1 0) 
;  (0 0 1 0 0 0 0 0 0 0 0 1 0 1 1 1))

; (assemble prog1 5) =>
; '((0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0) 
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1) 
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; (assemble prog2 0) =>
; '((0 1 0 1 0 0 0 0 0 0 0 0 1 0 1 1) 
;  (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1) 
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
;  (0 1 1 1 0 0 0 0 0 0 0 0 0 1 0 1) 
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
;  (1 1 0 0 0 0 0 0 0 0 0 0 1 1 0 0) 
;  (0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0) 
;  (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 0) 
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 1 0 0) 
;  (0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0) 
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
;  (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1) 
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; Here is a possibly useful table giving the equivalence
; of symbolic opcodes and their 4 bit translations.
; (Note that it can be used two ways, with left-lookup
; and right-lookup.)

(define op-table
  '((halt (0 0 0 0)) (load (0 0 0 1)) (store (0 0 1 0))
    (add (0 0 1 1)) (sub (0 1 0 0)) (input (0 1 0 1))
    (output (0 1 1 0)) (jump (0 1 1 1)) (skipzero (1 0 0 0))
    (skippos (1 0 0 1)) (skiperr (1 0 1 0)) (loadi (1 0 1 1))
    (storei (1 1 0 0)) (halt (1 1 0 1)) (halt (1 1 1 0))
    (halt (1 1 1 1))))



(define assemble
  (lambda (prog address)
    (aux-ass prog address prog address)))

(define (aux-ass prog address master m-address)
  (if (null? prog)
        '()
        (if (label? (first (first prog)))
            (if (null? (rest (rest (first prog))))
                (cons (append (left-lookup (first (rest (first prog))) op-table) (fill 12 '())) (aux-ass (rest prog) (+ 1 address) master m-address))
                (if (equal? (first (rest (first prog))) 'data)
                    (if (number? (first (rest (rest (first prog)))))
                        (cons (fill 16 (first (rest (tcint->bits (first (rest (rest (first prog)))))))) (aux-ass (rest prog) (+ 1 address) master m-address))
                        (cons (fill 16 (first (rest (tcint->bits (left-lookup (string->symbol (string-append (symbol->string (first (rest (rest (first prog))))) ":")) (symbol-table master m-address)))))) (aux-ass (rest prog) (+ 1 address) master m-address)))
                    (if (number? (first (rest (rest (first prog)))))
                        (cons (append (left-lookup (first (rest (first prog))) op-table) (list-tail (first (rest (tcint->bits (first (rest (rest (first prog))))))) 4)) (aux-ass (rest prog) (+ 1 address) master m-address))
                        (cons (append (left-lookup (first (rest (first prog))) op-table) (list-tail (first (rest (tcint->bits (left-lookup (string->symbol (string-append (symbol->string (first (rest (rest (first prog))))) ":")) (symbol-table master m-address))))) 4)) (aux-ass (rest prog) (+ 1 address) master m-address)))))
            (if (null? (rest (first prog)))
                (cons (append (left-lookup (first (first prog)) op-table) (fill 12 '())) (aux-ass (rest prog) (+ 1 address) master m-address))
                (if (equal? (first (first prog)) 'data)
                    (cons (fill 16 (first (rest (tcint->bits (first (rest (first prog))))))) (aux-ass (rest prog) (+ 1 address) master m-address))
                    (if (number? (first (rest (first prog))))
                        (cons (append (left-lookup (first (first prog)) op-table) (list-tail (first (rest (tcint->bits (first (rest (first prog)))))) 4)) (aux-ass (rest prog) (+ 1 address) master m-address))
                        (cons (append (left-lookup (first (first prog)) op-table) (list-tail (first (rest (tcint->bits (left-lookup (string->symbol (string-append (symbol->string (first (rest (first prog)))) ":")) (symbol-table master m-address))))) 4)) (aux-ass (rest prog) (+ 1 address) master m-address))))))))
  

; Next, we develop a simulator for the TC-201

; A configuration of the TC-201 will consist
; of a table giving the contents of the
; accumulator, program counter, run flag,
; arithmetic error bit, and memory locations.

; Entries are as follows:
; 1) accumulator: key is the symbol 'acc, value is
; a list of 16 bits giving its contents
; 2) program counter: key is the symbol 'pc, value is
; a list of 12 bits giving its contents
; 3) run flag: key is the symbol 'run-flag, value is
; 1 (for running) or 0 (for halted)
; 4) arithmetic error bit: key is the symbol 'aeb, value is
; 1 (for error) or 0 (for no error)
; 5) a memory location: key is a nonnegative
; number between 0 and 4095 giving a memory address,
; value is a list of 16 bits giving the
; contents of that memory location.  Those memory
; locations not listed are assumed to have
; 16 zero bits.

; Here's the configuration that results from
; assembling and loading program prog1 starting
; at memory location 0.

; (Recall the definition of prog1 from above.)

; (define prog1
;   '((start: load one)
;     (store count)
;     (halt)
;     (one: data 1)
;     (count: data 0)))


(define config1
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
  (pc (0 0 0 0 0 0 0 0 0 0 0 0)) 
  (run-flag 1) 
  (aeb 0) 
  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
  (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)) 
  (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))


; ** problem 8 ** (9 points)
; Write a procedure (init-config insts address)
; that takes a list of instructions insts (represented
; as a list of 16-bit lists) and a nonnegative
; integer memory address, and returns the configuration of
; the TC-201 with those instructions loaded into
; memory *starting at that address*, the accumulator
; zeroed, the program counter set to the address,
; the run-flag set to 1, and the arithmetic error bit set
; to 0.

; Here's a program to print a list with one top-level item per line.
; It will help you look at configurations. (You may use this
; procedure and the newline, display and print-lst procedures it uses.)

(define print-lst
  (lambda (lst)
    (cond
     ((null? lst) (newline) #t)
     (else (display (first lst)) (newline) (print-lst (rest lst))))))

; These are the instructions: load 3, store 4:

(define insts0
   '((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
     (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)))

; These are the instructions: load 8, store 9,
; halt, data 1, data 6.

(define insts1
  '((0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0)
    (0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1) 
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 
    (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1) 
    (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0)))

 
; Examples
; (init-config insts0 0) =>
; '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
;  (pc (0 0 0 0 0 0 0 0 0 0 0 0)) 
;  (run-flag 1) 
;  (aeb 0) 
;  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
;  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)))


; (init-config insts1 5) => 
; '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
;  (pc (0 0 0 0 0 0 0 0 0 1 0 1)) 
;  (run-flag 1) 
;  (aeb 0) 
;  (5 (0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0)) 
;  (6 (0 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1)) 
;  (7 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (8 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)) 
;  (9 (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0)))



(define init-config
  (lambda (insts address)
    (append (list (list 'acc (fill 16 '()))) (list (list 'pc (list-tail (first (rest (tcint->bits address))) 4))) (list (list 'run-flag 1)) (list (list 'aeb 0)) (aux-init-config insts address))))

(define (aux-init-config insts address)
  (cond [(null? insts) '()]
        [else (append (list (list address (first insts))) (aux-init-config (rest insts) (+ 1 address)))]))


; ** problem 9 ** (9 points)
; Write two procedures:

; (memory-read address config)
; that takes an address as a nonnegative integer
; and returns the list of 16 bits that is
; the contents of that memory location in config
; Note that if the address doesn't appear in the
; configuration as a key, the list of 16 zeroes
; should be returned.
; Hint: left-lookup should be useful here.

; (memory-write address contents config)
; that takes an address as a nonnegative integer
; and a contents as a list of 16 bits
; and a configuration config of the TC-201
; and returns the configuration with the
; contents of the given memory location changed
; to the given contents.
; Hint: update should be useful here.

; Examples
; (memory-read 0 config1) => '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
; (memory-read 1 config1) => '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
; (memory-read 2 config1) => '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
; (memory-read 7 config1) => '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)

; (memory-write 1 large16 config1)) =>
;    '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
;     (pc (0 0 0 0 0 0 0 0 0 0 0 0)) 
;     (run-flag 1) 
;     (aeb 0) 
;     (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
;     (1 (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
;     (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)) 
;     (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

; (memory-write 7 large16 config1)) =>
;    '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (pc (0 0 0 0 0 0 0 0 0 0 0 0))
;     (run-flag 1)
;     (aeb 0)
;     (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;     (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;     (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))
;     (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;     (7 (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))


(define memory-read
  (lambda (address config)
    (if (false? (left-lookup address config))
        (fill 16 '())
        (left-lookup address config))))

(define memory-write
  (lambda (address contents config)
    (update (list address contents) config)))


; ** problems 10&11 (18 points)
; Write the procedure
; (next-config config)
; that returns the next configuration of the TC-201
; after executing one instruction.
; If the run-flag is 0, the input configuration 
; should be returned unchanged.
; If the run-flag is 1, the configuration that
; results from executing the instruction at the address
; in the program counter should be returned.

; You will need to have input and output for your simulation.
; For input you can use the Racket procedure read. Here is a sample of how it works:
(define (sample-input)
  (display "input =")
         (read))
; Four output you can use the Racket procedure display.

; Here's a generic simulator to run a program
; once your next-config program is working.

(define simulate
  (lambda (config steps)
    (print-lst config)
    (if (< steps 1)
	'steps-exhausted
	(simulate (next-config config) (- steps 1)))))


(define next-config
  (lambda (config)
    (cond [(= (memory-read 'run-flag config) 0) config]
          [(and (= (memory-read 'run-flag config) 1) (equal? (memory-read (bits->tcint (left-lookup 'pc config)) config) (fill 16 '()))) (memory-write 'run-flag 0 config)]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(0 0 0 1)) (memory-write 'pc (extract 4 15 (first (rest (add-bits (fill 11 '(1)) (left-lookup 'pc config))))) (memory-write 'acc (memory-read (bits->tcint (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) config) config))]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(0 0 1 0)) (memory-write 'pc (extract 4 15 (first (rest (add-bits (fill 11 '(1)) (left-lookup 'pc config))))) (memory-write (bits->tcint (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) (left-lookup 'acc config) config))]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(0 0 1 1)) (if (equal? (first (add-bits (memory-read (bits->tcint (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) config) (left-lookup 'acc config))) 'ok)
                                                                                                             (memory-write 'pc (extract 4 15 (first (rest (add-bits (fill 11 '(1)) (left-lookup 'pc config))))) (memory-write 'aeb 0 (memory-write 'acc (first (rest (add-bits (memory-read (bits->tcint (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) config) (left-lookup 'acc config)))) config)))
                                                                                                             (memory-write 'pc (extract 4 15 (first (rest (add-bits (fill 11 '(1)) (left-lookup 'pc config))))) (memory-write 'aeb 1 (memory-write 'acc (fill 16 '()) config))))]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(0 1 0 0)) (if (equal? (first (sub-bits (left-lookup 'acc config) (memory-read (bits->tcint (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) config))) 'ok)
                                                                                                             (memory-write 'pc (extract 4 15 (first (rest (add-bits (fill 11 '(1)) (left-lookup 'pc config))))) (memory-write 'aeb 0 (memory-write 'acc (first (rest (sub-bits (left-lookup 'acc config) (memory-read (bits->tcint (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) config)))) config)))
                                                                                                             (memory-write 'pc (extract 4 15 (first (rest (add-bits (fill 11 '(1)) (left-lookup 'pc config))))) (memory-write 'aeb 1 (memory-write 'acc (fill 16 '()) config))))]
          
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(0 1 0 1)) (memory-write 'pc (extract 4 15 (first (rest (add-bits (fill 11 '(1)) (left-lookup 'pc config))))) (memory-write 'acc (tcint->bits (sample-input)) config))]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(0 1 1 0)) (display (bits->tcint (memory-read (bits->tcint (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) config)))]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(0 1 1 1)) (memory-write 'pc (fill 12 (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) config)]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(1 0 0 0)) (if (= 0 (bits->tcint (memory-read 'acc config)))
                                                                                                             (memory-write 'pc (extract 4 15 (first (rest (add-bits (left-lookup 'pc config) (fill 10 '(1 0)))))) config)
                                                                                                             (memory-write 'pc (extract 4 15 (first (rest (add-bits (left-lookup 'pc config) (fill 11 '(1)))))) config))]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(1 0 0 1)) (if (< 0 (bits->tcint (memory-read 'acc config)))
                                                                                                             (memory-write 'pc (extract 4 15 (first (rest (add-bits (left-lookup 'pc config) (fill 10 '(1 0)))))) config)
                                                                                                             (memory-write 'pc (extract 4 15 (first (rest (add-bits (left-lookup 'pc config) (fill 11 '(1)))))) config))]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(1 0 1 0)) (if (= 0 (memory-read 'aeb config))
                                                                                                             (memory-write 'pc (extract 4 15 (first (rest (add-bits (left-lookup 'pc config) (fill 11 '(1)))))) config)
                                                                                                             (memory-write 'aeb 0 (memory-write 'pc (extract 4 15 (first (rest (add-bits (left-lookup 'pc config) (fill 10 '(1 0)))))) config)))]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(1 0 1 1)) (memory-write 'pc (extract 4 15 (first (rest (add-bits (fill 11 '(1)) (left-lookup 'pc config))))) (memory-write 'acc (left-lookup (bits->tcint (left-lookup (bits->tcint (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) config)) config) config))]
          [(equal? (extract 0 3 (memory-read (bits->tcint (left-lookup 'pc config)) config)) '(1 1 0 0)) (memory-write 'pc (extract 4 15 (first (rest (add-bits (fill 11 '(1)) (left-lookup 'pc config))))) (memory-write (bits->tcint (left-lookup (bits->tcint (extract 4 15 (left-lookup (bits->tcint (left-lookup 'pc config)) config))) config)) (memory-read 'acc config) config))])))

; Test cases for each type of instruction are below,
; consisting of a test configuration and the desired result
; from next-config.

; This is config1, defined above, and the result, config1n.
; (define config1
; '((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
;   (pc (0 0 0 0 0 0 0 0 0 0 0 0)) 
;   (run-flag 1) 
;   (aeb 0) 
;   (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
;   (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
;   (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
;   (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)) 
;   (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

; Here's the result of running next-config on config1:
(define config1n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)) 
  (pc (0 0 0 0 0 0 0 0 0 0 0 1)) 
  (run-flag 1) 
  (aeb 0) 
  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
  (3 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)) 
  (4 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

; test of the run flag
; when the run-flag is 0, do nothing
(define config2 
'((acc (1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1)) 
  (pc (0 0 0 0 0 0 0 0 0 0 0 1)) 
  (run-flag 0) 
  (aeb 0) 
  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(define config2n config2)

; test of halt instruction:
; if the run-flag is on but the instruction is a halt,
; turn off the run flag and do not increment the program counter
(define config3 
'((acc (1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1)) 
  (pc (0 0 0 0 0 0 0 0 0 0 1 0)) 
  (run-flag 1) 
  (aeb 0) 
  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))

(define config3n
'((acc (1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1)) 
  (pc (0 0 0 0 0 0 0 0 0 0 1 0)) 
  (run-flag 0) 
  (aeb 0) 
  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (2 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))))
 
; load tests

; load from location listed in configuration
(define config4
'((acc (1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1)) 
  (pc (0 0 0 0 0 0 0 0 0 0 1 0)) 
  (run-flag 1) 
  (aeb 0) 
  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (2 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1))))

(define config4n
'((acc (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (pc (0 0 0 0 0 0 0 0 0 0 1 1)) 
  (run-flag 1) 
  (aeb 0) 
  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (2 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1))))
 
; load from location not listed in configuration
(define config5 
'((acc (1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1)) 
  (pc (0 0 0 0 0 0 0 0 0 0 1 0)) 
  (run-flag 1) 
  (aeb 0) 
  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (2 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config5n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)) 
  (pc (0 0 0 0 0 0 0 0 0 0 1 1)) 
  (run-flag 1) 
  (aeb 0) 
  (0 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (1 (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)) 
  (2 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; store test
; store in memory word listed in configuration
(define config6
'((acc (1 0 1 0 1 0 1 0 0 1 0 1 0 1 0 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1)) 
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)) 
  (11 (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0)) 
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config6n
'((acc (1 0 1 0 1 0 1 0 0 1 0 1 0 1 0 1))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0)) 
  (run-flag 1) 
  (aeb 0) 
  (10 (1 0 1 0 1 0 1 0 0 1 0 1 0 1 0 1))
  (11 (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; store, memory contents not explicitly in configuration
(define config7
'((acc (1 0 1 0 1 0 1 0 0 1 0 1 0 1 0 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1)) 
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))
  (11 (0 0 1 0 0 0 0 0 0 0 0 0 1 1 1 1))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config7n
'((acc (1 0 1 0 1 0 1 0 0 1 0 1 0 1 0 1))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0)) 
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))
  (11 (0 0 1 0 0 0 0 0 0 0 0 0 1 1 1 1))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))
  (15 (1 0 1 0 1 0 1 0 0 1 0 1 0 1 0 1))))
  
; add test, no error -- should turn off aeb
(define config8
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1)) 
  (run-flag 1) 
  (aeb 1) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
  (11 (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config8n
'((acc (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0)) 
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
  (11 (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; add test, error -- should turn on aeb and result in 0's
(define config9
'((acc (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1)) 
  (run-flag 1) 
  (aeb 0) 
  (10 (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
  (11 (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config9n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0)) 
  (run-flag 1) 
  (aeb 1) 
  (10 (0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
  (11 (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; sub test, no error -- should turn off aeb
(define config10
'((acc (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1)) 
  (run-flag 1) 
  (aeb 1) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
  (11 (0 1 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config10n
'((acc (0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 0))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0))
  (11 (0 1 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; sub test, error -- should turn on aeb and result in 0's
(define config11
'((acc (0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1)) 
  (run-flag 1) 
  (aeb 0) 
  (10 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
  (11 (0 1 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config11n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0)) 
  (run-flag 1) 
  (aeb 1) 
  (10 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
  (11 (0 1 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; input test
; interaction-dependent result

(define config-input
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 1) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (0 1 0 1 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))


; output test
; should print out 3

(define config-output
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (0 1 1 0 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

; jump test
(define config12
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (0 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config12n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 1 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (0 1 1 1 0 0 0 0 0 0 0 0 1 1 1 1))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 

; skipzero tests
; skipzero, accumulator is positive
(define config13
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))


(define config13n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; skipzero, accumulator is negative
(define config14
'((acc (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

 (define config14n
'((acc (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; skipzero, accumulator is zero
(define config15
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config15n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (pc (0 0 0 0 0 0 0 0 1 1 0 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; skippos tests
; skippos, accumulator is positive

(define config16
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config16n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 1 0 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; skippos, accumulator is negative
(define config17
'((acc (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

 (define config17n
'((acc (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; skippos, accumulator is zero
(define config18
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config18n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

; skiperr tests
; skiperr, aeb off
(define config19 
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config19n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; skiperr, aeb on (skips and turns it off)

(define config20
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 1) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))

(define config20n
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (pc (0 0 0 0 0 0 0 0 1 1 0 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (11 (1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))))
 
; loadi test
(define config21
'((acc (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1))
  (11 (1 0 1 1 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))
  (13 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))))

(define config21n
'((acc (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1))
  (11 (1 0 1 1 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))
  (13 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))))

; storei test
(define config22
'((acc (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 0 1 1))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1))
  (11 (1 1 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))
  (13 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0))))

(define config22n
'((acc (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1))
  (pc (0 0 0 0 0 0 0 0 1 1 0 0))
  (run-flag 1) 
  (aeb 0) 
  (10 (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1))
  (11 (1 1 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
  (12 (0 0 0 1 0 0 0 0 0 0 0 0 0 1 1 1))
  (13 (0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1))))

; list of all the test configurations (except for input and output)
(define test-configs 
  (list config1 config2 config3 config4 config5 config6
	config7 config8 config9 config10 config11 config12
	config13 config14 config15 config16 config17 config18
	config19 config20 config21 config22))

; list of all the desired results (except for input and output)
(define n-test-configs
  (list config1n config2n config3n config4n config5n config6n
	config7n config8n config9n config10n config11n config12n
	config13n config14n config15n config16n config17n config18n
	config19n config20n config21n config22n))
