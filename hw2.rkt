#lang racket

(provide hours
         simulate
         i-state i-symbol i-new-state i-new-symbol i-direction
         tm1
         tm-mystery
         i-match? i-find
         config1
         c-state c-head-position c-tape
         c-symbol change-state halted?
         write-symbol
         move-head
         next-config
         tm-sub tm-pal)

; Please DO NOT MODIFY the lines above this comment.
; They are needed for the autograder.

; This file may be loaded into Racket 
; Lines beginning with semicolons are Racket comments.

; Name: Isaac Pena
; Email address: isaac.pena@yale.edu

; CS 201a HW #2  DUE ** Sept 26 paper, Sept 27 electronic 2014 **

; Code due via "submit" by 11:59 pm September 27, 2014
; Portion written on paper due in class Sept 26 2014
; 5 pts per day will be deducted for lateness


;--------------------------------------------------------------------------------------------

; Please complete each problem below. There are two different kinds of 
; problems, those that require you to write code, and those 
; that require you to answer a question. 

; For problems that require code, enter it in this Racket file,
; and turn it in using the submit system by the due date.  

; For the problem that require an answer to a question (Prob 1), please write the answer 
; on to a physical piece of paper and hand it in during class on Sept 26,2014.   

; Note that parts of this file may be graded by an automatic process that will 
; fail if procedures are spelled incorrectly. Other parts of this file 
; may be graded by a human process which could be affected by the comments 
; in your code. In other words, it does not hurt to comment early and comment 
; often.

; If you are asked to write a procedure, you may write auxiliary procedures
; -- for each of your auxiliary procedures, please include a comment 
; explaining what it does.

; You may assume that the input is given as specified, you do not need to
; include tests in your code to verify that the input is valid. For example,
; if the problem states that the given argument is an integer, you do not need to
; test that it is an integer.

; You should program in a functional style, (i.e. as we have been doing in class) . 
; You should only use the subset of Racket that we have been working with in class.

;-------------------------------------------------------------------------------------------------

; Note: Learning to program a Turing machine is a lot like learning any other language. 
; You need to spend time learning both the syntax of the language as well as the paradigms of 
; thinking that the language requires. If you get stuck, remember that half of programing 
; is reducing complex problems into smaller, simpler, ones. The same is true for programing
; Turing Machines. As always we are here to help you if you need it.  

;------------------------------------------------------------------------------------------------

; ** problem 0 ** (1 easy point) 
; How many hours did you spend on this assignment, including reading. Replace the -1 
; with your number in this define statement
(define hours -1 )

; ----------------------------------------------------------------------------------------------------

; Turing machines were described in the lectures; see also the lecture notes on the course web page.
; Here is a top-level procedure to simulate a Turing machine starting from a given configuration, 
; printing out the successive configurations of the machine, until it halts.

(define (simulate machine config)
    (display config) (newline)
    (if (halted? machine config)
	'halted
	(simulate machine (next-config machine config))))

; machine is a representation of a Turing machine, and config is a representation of a configuration
; of the machine, both described below.

; The procedures halted? and next-config will be developed in problems 2-6 below; you will then
; have a complete Turing machine simulator.

; A Turing machine is represented as a list of instructions, where each instruction is a 5-tuple, 
; represented as a list: 
;           (current state, current symbol, new state, new symbol,move direction)

; The current state and new state should be Racket symbols, the current symbol and new symbol should
; be Racket symbols or numbers, and the move direction must be either the symbol L or the symbol R.
;

; Example: (q1 0 q3 1 L)
; is an instruction with current state q1, current symbol 0,new state q3, new symbol 1,and move
; direction L.

; Here are selectors for the parts of an instruction: Use them in your code -- they will be a 
; lot more mnemonic than the corresponding list-refs or firsts and rests

(define (i-state inst) (list-ref inst 0))
(define (i-symbol inst) (list-ref inst 1))
(define (i-new-state inst) (list-ref inst 2))
(define (i-new-symbol inst) (list-ref inst 3))
(define (i-direction inst) (list-ref inst 4))


; A Turing machine is simply a list of instructions.

; Example: a Turing machine that when started in state q1  on the leftmost of a string of 
; 0's and 1's changes all the 0's to 1's and ; all the 1's to 0's  and then returns the head 
; to the leftmost symbol and halts.

(define tm1 (list
	     '(q1 0 q1 1 R)
	     '(q1 1 q1 0 R)
	     '(q1 b q2 b L)
	     '(q2 0 q2 0 L)
	     '(q2 1 q2 1 L)
	     '(q2 b q3 b R)))


;-------------------------------------------------------------------------------------------------------


; ** problem 1 (10 points)
; Here is, in the *given format*, a Turing machine named tm-mystery.The machine takes a string 
; with only 1s and 0s as its input, and returns a string with only 1s and 0s as  its output. 
; (Blank symbols are not considered part of its output)

; Give a description of what operation this machine performs and describe how it
; performs it. Include an explanation of the meaning of each of its states. 

; NOTE: You can still do this assignment without a working simulator by simulating
; the machine by hand. 

; HINT: Try the strings:
; 01
; 10
; 110
; 001
; 1010

(define tm-mystery
(list '(q1 b q5 b L) '(q1 0 q2 x L) '(q1 1 q3 x L) '(q1 x q1 x R)
      '(q2 0 q2 0 L) '(q2 1 q2 1 L) '(q2 b q4 0 R) '(q2 x q2 x L)
      '(q3 0 q3 0 L) '(q3 1 q3 1 L) '(q3 b q4 1 R) '(q3 x q3 x L)
      '(q4 0 q4 0 R) '(q4 1 q4 1 R) '(q4 x q1 x R)
      '(q5 x q5 b L) '(q5 0 q5 0 L) '(q5 1 q5 1 L) '(q5 b q6 b R)))


;-------------------------------------------------------------------------------------------------------

; ** problem 2 (10 points)
; Write the following two procedures. Please use the instruction selectors defined above:
; i-state, i-symbol, i-new-state, i-new-symbol, i-direction

; (i-match? state symbol inst)
; returns #t if state and symbol are equal to  the state and symbol of instruction inst
; otherwise returns #f

; (i-find state symbol machine)
; returns #f if no instruction of Turing machine has state and symbol equal to state
; and symbol otherwise returns the first instruction in machine that matches.

; Examples:
; (i-match? 'q1 'b '(q1 b q2 b L)) => #t
; (i-match? 'q1  0 '(q1 1 q4 1 L)) => #f

; (i-find 'q1 1 tm1) => (q1 1 q1 0 R)
; (i-find 'q2 'b tm1) => (q2 b q3 b R)
; (i-find 'q3 1 tm1) => #f


(define i-match?
  (lambda (state symbol inst)
    (if (and (equal? (i-state inst) state) (equal? (i-symbol inst) symbol))
        #t
        #f)))

(define i-find
  (lambda (state symbol machine)
    (cond [(and (equal? (i-state (first machine)) state) (equal? (i-symbol (first machine)) symbol)) (first machine)]
          [(empty? (rest machine)) #f]
          [else (i-find state symbol (rest machine))])))

;----------------------------------------------------------------------------------------------------

; A Turing machine tape is a nonempty list of items, each of which is a Racket number or symbol.
; The symbol 'b is special -- it stands for the blank tape square. Symbols not explicitly listed 
; are assumed to be blanks.

; Examples: two tapes

(define tape1 '(1 1 0 1 0))
(define tape2 '(b 1 1 b 0 1 1 b b))

; A configuration is a 3-tuple represented by a list containing
;     (state, head position, tape) 
; where state is a Racket symbol, head position is an integer greater than or equal to 0 and 
; less than (length tape), and tape is a non-empty Turing machine tape. This is zero-based 
; indexing: 0 stands for the head positioned on the leftmost symbol of the tape.

; Example: a configuration with state q1, head on the leftmost square, and tape contents tape1.

(define config1 (list 'q1 0 tape1)) 

; Here are selectors for the parts of a configuration.
; Use them in your code rather than firsts, list-refs, etc.

(define c-state (lambda (config) (list-ref config 0)))
(define c-head-position (lambda (config) (list-ref config 1)))
(define c-tape (lambda (config) (list-ref config 2)))

;--------------------------------------------------------------------------------------------------------

; ** problem 3 ** (10 points)
; Write the following three procedures.

; (c-symbol config) 
; to return the symbol currently under the head in the given configuration config. Assume that the 
; head position is greater than or equal to 0 and less than the length of the tape.

; (change-state new-state config)
; to return a configuration equal to config with the current state changed to new-state

; (halted? machine config)
; that returns #t if Turing machine 'machine' is halted in configuration config (ie, no instruction of the
; machine matches the current state and symbol in configuration) and #f otherwise.

; Examples:
; (c-symbol '(q6 3 (0 0 0 1 0 0 b))) => 1
; (c-symbol '(q4 6 (0 0 0 1 0 0 b))) => b
; (c-symbol config1) => 1
; (change-state 'q2 '(q6 3 (0 0 1 1 b))) => (q2 3 (0 0 1 1 b))
; (change-state 'q1 config1) => (q1 0 (1 1 0 1 0))
; (halted? tm1 config1) => #f
; (halted? tm1 '(q3 0 (b 0 0 1 0 1 b))) => #t


(define c-symbol
  (lambda (config)
    (list-ref (c-tape config) (c-head-position config))))

(define change-state
  (lambda (new-state config)
    (list new-state (c-head-position config) (c-tape config))))

(define halted?
  (lambda (machine config)
    (cond [(and (equal? (c-state config) (i-state (first machine))) (equal? (c-symbol config) (i-symbol (first machine)))) #f]
          [(empty? (rest machine)) #t]
          [else (halted? (rest machine) config)])))

;-------------------------------------------------------------------------------------------------------


; ** problem 4 ** (10 points)
; Write a procedure (write-symbol s config) that returns a configuration equal to config
; with the currently scanned symbol replaced by the symbol s.

; Examples:
; (write-symbol 0 config1) => (q1 0 (0 1 0 1 0))
; (write-symbol 'b '(q6 3 (0 1 0 1 1))) => (q6 3 (0 1 0 b 1))
; (write-symbol 'a '(q2 2 (w x y z))) => (q2 2 (w x a z))




(define write-symbol
  (lambda (s config)
    (list (c-state config) (c-head-position config) (write-aux s config 0))))

;(write-aux) is used to count the indexes of the list as it goes, eventually replacing the index indicated by (c-head-position) with s and tacking that onto the rest of the list. 
; Quite honestly, this is only here because I couldn't figure out how to return the current index (and it wouldn't have meant much if i was recursively cutting off pieces with (rest))
(define (write-aux s config num)
  (if (equal? (c-head-position config) num) 
      (cons s (list-tail (c-tape config) (+ 1 (c-head-position config))))
      (cons (list-ref (c-tape config) num) (write-aux s config (+ 1 num)))))

;----------------------------------------------------------------------------------------------------------

; ** problem 5 ** (10 points)
; Write a procedure (move-head dir config) that returns the value of config with 
; the head moved one square in the direction dir (specified by symbol 'R or 'L)

; Make sure that if the head-position would "run off" the tape, that a blank is
; added to the tape to guarantee that the new head position is greater than or 
; equal to 0 and less than the length of the tape.

; You may find it useful to review let and let*. 

; Examples:
; (move-head 'R config1) => (q1 1 (1 1 0 1 0))
; (move-head 'L config1) => (q1 0 (b 1 1 0 1 0))
; (move-head 'R '(q6 3 (0 1 1 0))) => (q6 4 (0 1 1 0 b))

; I used the weird reverse(cons 'b (reverse format here because I wasn't sure if append use was kosher. This works, so although it's a little odd I feel ok about it.

(define move-head
  (lambda (dir config)
    (cond [(and (equal? 'L dir) (<= 1 (c-head-position config))) (list (c-state config) (- (c-head-position config) 1) (c-tape config))]
          [(and (equal? 'R dir) (> (length (c-tape config)) (+ (c-head-position config) 1))) (list (c-state config) (+ (c-head-position config) 1) (c-tape config))]
          [(equal? 'R dir) (list (c-state config) (+ (c-head-position config) 1) (reverse (cons 'b (reverse (c-tape config)))))]
          [(equal? 'L dir) (list (c-state config) (c-head-position config) (cons 'b (c-tape config)))])))
      
;---------------------------------------------------------------------------------------------      
	  
; ** problem 6 ** (15 points)
; Write a procedure (next-config mach config) that returns the next configuration for the
; Turing machine mach in configuration config If there is no applicable instruction, the 
; configuration remains the same.

; Hint: get halted?, i-find, change-state, move-head, and write-symbol
; working and use them!

; Examples:
; (next-config tm1 config1) => (q1 1 (0 1 0 1 0))
; (next-config tm1 '(q1 1 (0 1 0 1 0))) => (q1 2 (0 0 0 1 0))
; (next-config tm1 '(q1 4 (0 0 1 0 0))) => (q1 5 (0 0 1 0 1 b))
; (next-config tm1 '(q1 5 (0 0 1 0 1 b))) => (q2 4 (0 0 1 0 1 b))
; (next-config tm1 '(q3 1 (b 0 0 1 0 1 b))) => (q3 1 (b 0 0 1 0 1 b))


(define next-config
  (lambda (mach config)
    (if (halted? mach config)
        config
        (next-aux mach config (i-find (c-state config) (c-symbol config) mach)))))

(define (next-aux mach config next-instr)
  (move-head (i-direction next-instr) (change-state (i-new-state next-instr) (write-symbol (i-new-symbol next-instr) config))))
                                

;----------------------------------------------------------------------------------------------
; If your procedures are working, then you should
; be able to run the following example, which
; shows the successive configurations of Turing machine
; tm1 when run from configuration config1.

;; > (simulate tm1 config1)
;; (q1 0 (1 1 0 1 0))
;; (q1 1 (0 1 0 1 0))
;; (q1 2 (0 0 0 1 0))
;; (q1 3 (0 0 1 1 0))
;; (q1 4 (0 0 1 0 0))
;; (q1 5 (0 0 1 0 1 b))
;; (q2 4 (0 0 1 0 1 b))
;; (q2 3 (0 0 1 0 1 b))
;; (q2 2 (0 0 1 0 1 b))
;; (q2 1 (0 0 1 0 1 b))
;; (q2 0 (0 0 1 0 1 b))
;; (q2 0 (b 0 0 1 0 1 b))
;; (q3 1 (b 0 0 1 0 1 b))
;; halted
;-----------------------------------------------------------------------------------------------------------

; ** problem 7 (16 points)
; Define (in the given Racket repesentation) a Turing machine tm-sub to subtract 
; 1 in ternary (base 3).

; The input to the machine is a string of symbols 0, 1, 2 representing a positive 
; integer in ternary in the usual way, with no leading zeroes. (The first ten 
; positive integers in ternary are 1, 2, 10, 11, 12, 20, 21, 22, 100, 101.)
; The machine will be started in state q1 on the leftmost symbol of the input 
; string.

; The output of the machine should be the correct ternary representation  of the 
; integer that is 1 less than the input integer. (With no leading zeroes if the
; answer is positive.) When the machine halts, the head should be positioned on 
; the leftmost non-blank symbol of the output.

; IMPORTANT: Include a clear and careful  description of how your Turing
; machine works.

; NOTE: you can still do this problem if your simulator is not working, assuming
; you understand Turing machines and the Racket representation of them given above.'
; Examples of input => output
; 1 => 0
; 2  => 1
; 10 => 2
; 100 => 22
; 102 => 101
; 200 => 122


(define sub-config1 '(q1 0 (1)))
(define sub-config2 '(q1 0 (2)))
(define sub-config3 '(q1 0 (1 0 0)))
(define sub-config4 '(q1 0 (1 0 2 2 0 0)))


; The Turing machine tm-sub has 9 states. The machine's general operation is as follows:
; q1 moves to the far right of the input string, stopping and switching to q2 on the last element - i.e. the last non-blank in the string
; q2 does the actual subtraction, taking 1 from a 1 or a 2 and then swiching to q3, or changing a 0 to a 2 and 'carrying' by operating again on the next element to the left.
; q3, now that the subtraction is complete, moves back to the first element of the string, and switches to q4 once it gets there.
; q4 moves right along the string again, not touching 0s to determine if they're just leading or if they're actual contents of the string (really, only if the input is (1)).
;       q4 changes to q5 if it hits a non-zero element (so as to signify that the 0s are just leading) or q6 if it reaches a blank(the end - meaning that the 0 is the entire subtracted string.)
; q5, knowing that the zeros left of it (or on it) are leading, changes them to blank when it finds them, switching to q7 when it hits a blank, and moving right.
; q6, knowing that the zeros left of it (or on it - zero, singular, really) are content, leaves them when it finds them, switching to q7 when it hits a blank and moving right.
; q7 deals with the issue that we have left - the number of blanks on the left side is dependent on how the string is subtracted (i.e. whether the first digit needed carrying, etc.)
;      q7 is essentially the 'safety' state, that makes sure that the head position isn't in a mess of blanks leading the subtracted string. It copies and calls itself through blanks and
;      upon reaching the first non-blank element (be it 0, 1, or 2) it changes to state 8 and hops left to the last leading blank.
; q8 moves right once - automatically landing on the first non-blank element through the work of q7 - and then calls q9.
; q9 is halt.

(define tm-sub
  (list '(q1 0 q1 0 R) '(q1 1 q1 1 R) '(q1 2 q1 2 R) '(q1 b q2 b L)
        '(q2 0 q2 2 L) '(q2 1 q3 0 L) '(q2 2 q3 1 L) 
        '(q3 0 q3 0 L) '(q3 1 q3 1 L) '(q3 2 q3 2 L) '(q3 b q4 b R)
        '(q4 0 q4 0 R) '(q4 1 q5 1 L) '(q4 2 q5 2 L) '(q4 b q6 b L)
        '(q5 0 q5 b L) '(q5 1 q5 1 L) '(q5 2 q5 2 L) '(q5 b q7 b R)
        '(q6 0 q6 0 L) '(q6 1 q6 1 L) '(q6 2 q6 2 L) '(q6 b q7 b R)
        '(q7 0 q8 0 L) '(q7 1 q8 1 L) '(q7 2 q8 2 L) '(q7 b q7 b R)
        '(q8 0 q9 0 R) '(q8 1 q9 1 R) '(q8 2 q9 2 R) '(q8 b q9 b R)))

;------------------------------------------------------------------------------------------------------------------

; ** problem 8 ** (16 points)
; Define in the Racket format established above a Turing machine named tm-pal
; which when started on an input consisting of a string 0's and 1's, checks to 
; see whether the string is a palindrome (ie,is equal to its reverse.) If so, 
; it leaves a single 1 on the tape and halts; otherwise, it leaves a single 0 
; on the tape and halts. In both cases, the head is positioned on the single 
; non-blank symbol on the tape when the machine halts.


; IMPORTANT: Include a clear and careful description of how your Turing machine works.

; NOTE: you can still do this problem if your simulator is not working, assuming 
; you understand Turing machines and the Racket representation of them given above.


; Examples:
; input: 1111    output: 1
; input: 1001001 output: 1
; input: 0010    output: 0
; input: 0       output: 1
; input: empty   output: 1 (the empty string is a palindrome)


; If your simulator is working, include one run of your tm-pal as a comment.  Here are some sample configurations for testing:


(define pal-config1 '(q0 0 (1 1 1 1)))
(define pal-config2 '(q0 0 (1 0 0 1 0 0 1)))
(define pal-config3 '(q0 0 (0 0 1 0)))
(define pal-config4 '(q0 0 (0)))
(define pal-config5 '(q0 0 (b)))

; tm-pal's main method of operation involves chopping elements off at the front, making sure it's the same element as the back via use of states, and then chopping the back off, and repeating.
; The turing machine is essentially cyclical and states 1-6 repeat until the last element is reached. This is possible because removing the first and last of a palindrome is still a palindrome.
; q0 is the start of the loop - if it finds a 0 it switches to q2 and starts moving right, if it finds a 1 it switches to q3 and does the same, and if it hits a blank (indicating that the 
;        problem is finished as the end of the string (chopped off at front and back) has been reached, it leaves a 1 for success and moves into q7, which takes care of head position.
; q2 moves right across the string, stopping when it hits a blank, hopping left, and switching to q4. It is the version of this state for the "0" path.
; q3 works exactly as q2 does, although it switches to q5 when it ends. It is the "1" version. 
; q4 checks if the last element is a 0 - having only been called by a "0" version state, q2, and deletes it if it is, moving to q6. It switches to q8 if it finds and deletes a 1. 
;       If q4 hits a blank, it means that there's only a 1 or a 0 left in the list, automatically ensuring that the single element is a palindrome. As such, it activates q6 and moves left as with 0.
; q5 works exactly as q4 does, although it activates q6 on finding a 1 and q8 on finding a 0. It handles blanks exactly the same as q4, as explained in the above line.
; q6 returns the cycle back to q0, by moving left over the string until it reaches the first non-blank element, and then switching back to q0 so the above can be done on the potential sub-palindrome.
; q7 is a format state - it always starts just right of a single 0 or 1, on a blank, and just hops left and then switches to q9.
; q8 is activated once a palindrome match fails, deleting everything it finds, and once it hits a blank (nothing more to delete), it leaves a 0, and hops right, as does the blank-catching part of q0. It then switches to q7, which places the head correctly.
; q9 is the halt state.

; Had to awkwardly change q1 to q0 because test case formatting is different 9/27/14
(define tm-pal
  (list '(q0 0 q2 b R) '(q0 1 q3 b R) '(q0 b q7 1 R)
        '(q2 0 q2 0 R) '(q2 1 q2 1 R) '(q2 b q4 b L)
        '(q3 0 q3 0 R) '(q3 1 q3 1 R) '(q3 b q5 b L)
        '(q4 0 q6 b L) '(q4 1 q8 b L) '(q4 b q6 b L)
        '(q5 0 q8 b L) '(q5 1 q6 b L) '(q5 b q6 b L)
        '(q6 0 q6 0 L) '(q6 1 q6 1 L) '(q6 b q0 b R)
                                      '(q7 b q9 b L)
        '(q8 0 q8 b L) '(q8 1 q8 b L) '(q8 b q7 0 R)))

;And now for a test case:
;(simulate tm-pal pal-config2) returns the following:
;(q0 0 (1 0 0 1 0 0 1))
;(q3 1 (b 0 0 1 0 0 1))
;(q3 2 (b 0 0 1 0 0 1))
;(q3 3 (b 0 0 1 0 0 1))
;(q3 4 (b 0 0 1 0 0 1))
;(q3 5 (b 0 0 1 0 0 1))
;(q3 6 (b 0 0 1 0 0 1))
;(q3 7 (b 0 0 1 0 0 1 b))
;(q5 6 (b 0 0 1 0 0 1 b))
;(q6 5 (b 0 0 1 0 0 b b))
;(q6 4 (b 0 0 1 0 0 b b))
;(q6 3 (b 0 0 1 0 0 b b))
;(q6 2 (b 0 0 1 0 0 b b))
;(q6 1 (b 0 0 1 0 0 b b))
;(q6 0 (b 0 0 1 0 0 b b))
;(q0 1 (b 0 0 1 0 0 b b))
;(q2 2 (b b 0 1 0 0 b b))
;(q2 3 (b b 0 1 0 0 b b))
;(q2 4 (b b 0 1 0 0 b b))
;(q2 5 (b b 0 1 0 0 b b))
;(q2 6 (b b 0 1 0 0 b b))
;(q4 5 (b b 0 1 0 0 b b))
;(q6 4 (b b 0 1 0 b b b))
;(q6 3 (b b 0 1 0 b b b))
;(q6 2 (b b 0 1 0 b b b))
;(q6 1 (b b 0 1 0 b b b))
;(q0 2 (b b 0 1 0 b b b))
;(q2 3 (b b b 1 0 b b b))
;(q2 4 (b b b 1 0 b b b))
;(q2 5 (b b b 1 0 b b b))
;(q4 4 (b b b 1 0 b b b))
;(q6 3 (b b b 1 b b b b))
;(q6 2 (b b b 1 b b b b))
;(q0 3 (b b b 1 b b b b))
;(q3 4 (b b b b b b b b))
;(q5 3 (b b b b b b b b))
;(q6 2 (b b b b b b b b))
;(q0 3 (b b b b b b b b))
;(q7 4 (b b b 1 b b b b))
;(q9 3 (b b b 1 b b b b))

;;;; end of hw2.rkt
