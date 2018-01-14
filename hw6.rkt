#lang racket
; 
;
(provide
 hours
 product
re-string?
re-concatenation? 
re-union? 
re-star? 
op
arg
args
pick
pick-string
reverse-exp
accepts?
cfg-mine
generate
 )
; DO NOT CHANGE THE LINES ABOVE THIS COMMENT
;
; ********************************************************************
; Name: Isaac Pena
; Email address: isaac.pena@yale.edu
; ********************************************************************

; CS 201a HW #6  DUE Friday, November 21, 2014 at 11:59 pm
; electronically, using the submit command.  
; Lateness penalties (5 points per day) will apply.
;
; This assignment uses Racket (not R5RS), set the
; Be sure to set the language option correctly in Dr. Racket.

; Topics: strings, regular expressions, finite-state
; acceptors, and context free grammars

; Unless the problem specifies otherwise:
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.
; * Your procedures need *not* fail gracefully if their
; inputs do not satisfy the assumptions of the problem.
; * Please use the specified names (and numbers of arguments)
; for the required procedures in a problem; this is to
; facilitate testing.
;
; You will need to use the built in function "random", rather than
; the object to generate random numbers that we used in HW#5. 
;
; Do not use mutators in this assignments. Use the procedures allowed
; in hw1 to hw4 (i.e. reverse, append, map etc.). In addition, 
; you may use the procedures "apply" and "member". You can look these
; up in the Racket Guide.


; ********************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment (including reading):

(define hours 10)

; ********************************************************************
; Representation of finite strings
; ********************************************************************
; Rather than use Racket strings to represent strings, which
; would limit the set of symbols to ASCII characters, we
; define our own representation of Finite Strings as follows.

; A Finite String of symbols is represented by a list of Racket symbols.
; Thus, (a b a b b a) represents a Finite String with six symbols,
; the first, third and sixth being a's and the rest b's.
; We'll also consider (the dog barked at the other dog) 
; as a Finite String of seven symbols.
; The empty Finite String is denoted by the empty list ().
; Note that (append str1 str2) concatenates two
; Finite Strings str1 and str2.
; 
; ********************************************************************
; ** problem 1 ** (11 points)
; Define a procedure (product lst1 lst2)
; that takes two lists lst1 and lst2 of Finite Strings
; and returns a list of all the strings that
; can be obtained by concatenating a string
; from lst1 with a string from lst2.
; The returned list should contain no duplicates.

; Examples
; (product '() '((a) (a b))) => '()
; (product '(()) '((a) (a b))) => '((a) (a b))
; (product '((a) (a b)) '((c d))) => '((a c d) (a b c d))
; (product '((a) (a a)) '((a) (a a))) => '((a a) (a a a) (a a a a))
; (product '((a b) (b c)) '((a b) (d e))) =>
;    '((a b a b) (a b d e) (b c a b) (b c d e))
; (product '(()) '(())) => '(())
; (product '((the)) '((small dog) (large cat))) =>
;     '((the small dog) (the large cat))
; (product '((small) (large)) '((dog) (cat))) =>
;     '((small dog) (small cat) (large dog) (large cat))


(define product
  (lambda (lst1 lst2)
    (if (null? lst1)
        '()
        (remove-duplicates (product-aux lst1 lst2 lst2)))))


(define product-aux
  (lambda (lst1 lst2 org2)
    (if (null? lst1)
        '()
        (if (null? (rest lst2))
            (cons (append (first lst1) (first lst2)) (product-aux (rest lst1) org2 org2))
            (cons (append (first lst1) (first lst2)) (product-aux lst1 (rest lst2) org2))))))
  


; ********************************************************************
; Representation of Regular Expressions
; ********************************************************************
; Regular Expressions will be represented as follows.
; 1) A Finite String (defined above) is a regular expression; for these
; purposes, the string may not contain the symbols !, +, or *.
; 2) The concatenation of two or more expressions is represented by
; a list starting with the symbol ! and containing the expressions.
; 3) The union of two or more expressions is represented
; by a list starting with + and containing the expressions.
; 4) The Kleene star of one expression is represented by a list
; starting with * and containing the expression to be starred.

; Some examples of regular expressions in this representation:

(define exp0 '())
(define exp1 '(a a b a b))
(define exp2 '(+ (a b) (b)))
(define exp3 '(! (* (b)) (a) (* (+ (b) (! (a) (* (b)) (a))))))
(define exp4 '(! (alas) (+ (and) (or)) (alack)))
(define exp5 '(! (the sleeping) (+ (dog) (cat) (beast))))
(define exp6 '(* (+ (a) (b))))
(define exp7 '(! (a) (* (+ (very) (quite))) (long) (+ (story) (tale))))

; ********************************************************************
; ** problem 2 ** (11 points)
; Write predicate and selector procedures for regular expressions exp.
; You may assume that the input is Regular Expression defined as above.

; (re-string? exp) should return #t iff exp is a Finite String
; (re-concatenation? exp) should return #t iff exp is a concatenation
; (re-union? exp) should return #t iff exp is a union
; (re-star? exp) should return #t iff exp is a Kleene star expression


; If exp is a concatenation then
; (op exp) should return the symbol ! and (args exp) should return the list
; of its arguments
; If exp is a union then
; (op exp) should return the symbol + and (args exp) should return the list
; of its arguments
; If exp is a Kleene star expression, then
; (op exp) should return the symbol * and (arg exp) should return its argument

; After this problem, use these procedures to test the type and
; refer to the parts of regular expressions.

; Examples
; (re-string? exp0) => #t
; (re-string? exp1) => #t
; (re-string? exp2) => #f
; (re-string? exp4) => #f
; (re-concatenation? exp0) => #f
; (re-concatenation? exp1) => #f
; (re-concatenation? exp2) => #f
; (re-concatenation? exp3) => #t
; (re-concatenation? exp6) => #f
; (re-union? exp0) => #f
; (re-union? exp1) => #f
; (re-union? exp2) => #t
; (re-union? exp3) => #f
; (re-star? exp0) => #f
; (re-star? exp1) => #f
; (re-star? exp2) => #f
; (re-star? exp6) => #t
; (op exp2) => '+
; (op exp3) => '!
; (op exp6) => '*
; (args exp2) => '((a b) (b))
; (args exp3) => '((* (b)) (a) (* (+ (b) (! (a) (* (b)) (a)))))
; (arg exp6) => '(+ (a) (b))


(define re-string?
  (lambda (exp)
    (if (null? exp)
        #t
        (if (list? (first exp))
            (and (re-string? (first exp)) (re-string? (rest exp)))
            (if (or (equal? (first exp) '!) (equal? (first exp) '+) (equal? (first exp) '*))
                #f
                #t)))))

(define re-concatenation?
  (lambda (exp)
    (if (null? exp)
        #f
        (if (equal? (first exp) '!)
            #t
            #f))))

(define re-union?
  (lambda (exp)
   (if (null? exp)
        #f
        (if (equal? (first exp) '+)
            #t
            #f))))

(define re-star?
  (lambda (exp)
    (if (null? exp)
        #f
        (if (equal? (first exp) '*)
            #t
            #f))))

(define op 
  (lambda (exp)
    (if (null? exp)
        #f
        (first exp))))

(define args
  (lambda (exp)
    (if (null? exp)
        #f
        (rest exp))))

(define arg 
  (lambda (exp)
    (if (or (null? exp) (null? (rest exp)))
        #f
        (first (rest exp)))))


; ********************************************************************
; ** problem 3 ** (11 points)
; Write two procedure (pick lst) and (pick-string exp)
; as follows.

; (pick lst) takes a list lst and
; returns a uniformly chosen random element from lst.
; If lst is the null list, your procedure should return #f.

; Note: the   Racket library procedure
; (random n) returns a randomly chosen
; integer between 0 and n-1 inclusive.

; (pick-string exp) takes a regular expression exp
; and returns a randomly chosen Finite String in the
; language of exp.  Just how you do this is up to you,
; but your procedure should be capable of generating
; any Finite String in the language, and none that aren't.
; If there is no limit on the length of strings that can be 
; generated in the language, there should be no limit on the
; length of string returnd=ed by pick-string.

; Hint: When you pick from a Kleene star expression there should
; be some probability that the empty string will be selected.

; You will want to use recursion on the structure of the
; expression for pick-string.

; Examples (Your mileage may vary!):
; (pick '(a b c)) => 'a
; (pick '(a b c)) => 'c
; (pick '(b)) => 'b
; (pick '(a a a b b b c c)) => 'b
; (pick '(5 7 9)) => 5
; (pick '(5 7 9)) => 9
; (pick '()) => '()
; (pick-string exp0) => '()
; (pick-string exp1) => '(a a b a b)
; (pick-string exp2) => '(b)
; (pick-string exp2) => '(a b)
; (pick-string exp3) => '(b a b a a)
; (pick-string exp3) => '(b b b b a b b b a b b b b b a)
; (pick-string exp3) => '(a)
; (pick-string exp4) => '(alas or alack)
; (pick-string exp4) => '(alas and alack)
; (pick-string exp5) => '(the sleeping dog)
; (pick-string exp5) => '(the sleeping cat)
; (pick-string exp6) => '(b a b a a a)
; (pick-string exp6) => '(a a a b)
; (pick-string exp6) => '(b b)
; (pick-string exp7) => '(a quite quite very very very long tale)
; (pick-string exp7) => '(a long story)

(define pick
  (lambda (lst)
    (if (null? lst)
        #f
        (list-ref lst (random (length lst))))))

(define kleene-star
  (lambda (exp)
    (let ((n (random 3)))
      (if (= n 0)
          '(())
          (product (list (pick-string (first (rest exp)))) (kleene-star exp))))))

(define pick-string
  (lambda (exp)
    (cond ((re-string? exp) exp)
          ((re-union? exp) (pick-string (pick (rest exp))))
          ((re-concatenation? exp) (flatten (map pick-string (rest exp))))
          ((re-star? exp)  (first (kleene-star exp))))))


; ********************************************************************
; ** problem 4 ** (11 points)
; Write a procedure (reverse-exp exp) 
; to take a regular expression exp
; (in the representation above) and return another
; regular expression exp1 such that L(exp1) contains
; exactly the reverses of all the strings in L(exp).
; (Therefore the reverse of a regular language is regular.)

; Examples
; (reverse-exp exp0) => '()
; (reverse-exp exp1) => '(b a b a a)
; (pick-string (reverse-exp exp2)) => '(b a)
; (pick-string (reverse-exp exp3)) => '(a b b b b b b b b b b a a)
; (pick-string (reverse-exp exp3)) => '(a)
; (pick-string (reverse-exp exp4)) => '(alack and alas)
; (pick-string (reverse-exp exp4)) => '(alack or alas)
; (pick-string (reverse-exp exp5)) => '(dog sleeping the)
; (pick-string (reverse-exp exp6)) => '(a a a b b)
; (pick-string (reverse-exp exp6)) => '()
; (pick-string (reverse-exp exp7)) => '(tale long a)
; (pick-string (reverse-exp exp7)) => '(story long quite very quite a)

(define homebrew-reverse
  (lambda (lst lst2)
    (if (null? lst)
        lst2
        (homebrew-reverse (rest lst) (cons (first lst) lst2)))))

(define kleene-rev
  (lambda (exp)
    (let ((n (random 3)))
      (if (= n 0)
          '(())
          (product (list (reverse-exp (first (rest exp)))) (kleene-rev exp))))))

(define reverse-exp
  (lambda (exp)
    (cond ((re-string? exp) (homebrew-reverse exp '()))
          ((re-union? exp) (cons '+ (map reverse-exp (rest exp))))
          ((re-concatenation? exp) (cons '! (homebrew-reverse (map reverse-exp (rest exp)) '())))
          ((re-star? exp) (cons '* (map reverse-exp (rest exp)))))))


; ********************************************************************
; Representation of finite state acceptors
; ********************************************************************
; A Finite State Acceptor is represented by a list of
; three items: the start state, the list of accepting states,
; and a table giving the transition function.  Each entry
; in the table consists of a key (a list consisting
; of a state and a symbol) and a value (a state).

; For example, here is a Finite State Acceptor
; that accepts all strings of a's and b's with
; an odd number of a's:

(define fsa1 
  '(q0 (q1) (((q0 a) q1)  ((q1 a) q0)
	     ((q0 b) q0)  ((q1 b) q1))))

; Here is another acceptor, which accepts strings of a's and strings
; of b's, but no string that contains both a's and b's.

(define fsa2
  '(q0 (q0 q1 q2) (((q0 a) q1)  ((q1 a) q1)  ((q2 a) q3)  ((q3 a) q3)
		   ((q0 b) q2)  ((q1 b) q3)  ((q2 b) q2)  ((q3 b) q3))))


; ********************************************************************
; ** problem 5 ** (11 points)
; Write a procedure (accepts? fsa str)
; that takes a finite state acceptor fsa (as just specified)
; and a Finite String str,
; and returns #t if the acceptor accepts the string, #f otherwise.

; Examples:
; (accepts? fsa1 '(a b b a b a)) => #t
; (accepts? fsa1 '(b b b)) => #f
; (accepts? fsa1 '(b a b a)) => #f
; (accepts? fsa1 '(a)) => #t
; (accepts? fsa2 '()) => #t
; (accepts? fsa2 '(a a a a)) => #t
; (accepts? fsa2 '(a a b b)) => #f
; (accepts? fsa2 '(b b)) => #t
; (accepts? fsa2 '(b a b)) => #f

(define accepts?
  (lambda (fsa str)
    (if (null? str)
        (if (member (first fsa) (first (rest fsa)))
            #t
            #f)
        (if (member (accepts-aux (first fsa) str (first (rest (rest fsa)))) (first (rest fsa)))
            #t
            #f))))

(define is-in
  (lambda (state char rules)
    (if (equal? (first (first rules)) (list state char))
        (first (rest (first rules)))
        (is-in state char (rest rules)))))

(define accepts-aux
  (lambda (state str rules)
    (if (null? str)
        state
        (accepts-aux (is-in state (first str) rules) (rest str) rules))))
        
        


; ********************************************************************
; ** problem 6 (11 points)
; Define a procedure (complement fsa)
; that takes a Finite State Acceptor
; and returns a Finite State Acceptor
; that accepts the strings that fsa
; rejects and vice versa.
; (Therefore the complement of a regular language is regular.)

; Examples
; (accepts? (complement fsa1) '(a b b a b a)) => #f
; (accepts? (complement fsa1) '(b b b)) => #t
; (accepts? (complement fsa2) '()) => #f
; (accepts? (complement fsa2) '(a a a a)) => #f
; (accepts? (complement fsa2) '(a a b b)) => #t



(define complement
  (lambda (fsa)
    (list (first fsa) (complement-aux (first (rest fsa)) (remove-duplicates (grab-states (first (rest (rest fsa)))))) (first (rest (rest fsa))))))

(define grab-states
  (lambda (rules)
    (if (null? rules)
        '()
        (cons (first (first (first rules))) (grab-states (rest rules))))))

(define is-in-states
  (lambda (acc-states ext-state)
    (if (null? acc-states)
        #f
        (if (equal? (first acc-states) ext-state)
            #t
            (is-in-states (rest acc-states) ext-state)))))
    

(define complement-aux
  (lambda (acc-states ext-states)
    (if (null? ext-states)
        '()
        (if (is-in-states acc-states (first ext-states))
            (complement-aux acc-states (rest ext-states))
            (cons (first ext-states) (complement-aux acc-states (rest ext-states)))))))
            


; ********************************************************************
; Representation of context free grammars
; ********************************************************************
; A Context Free Grammar is a list of two items:
; the start symbol of the grammar, and the list
; of rules of the grammar.

; Each rule is a list of two items:
; a nonterminal of the grammar (the lefthand side of the rule) and
; a Finite String of terminals or nonterminals of the grammar
; (the righthand side of the rule).
; We will assume that the nonterminals of the grammar
; are those that appear on the lefthand side of a rule, and
; that symbols that appear on the righthand side of some
; rule but not on the lefthand side of any rule are terminal symbols.

; Here are the rules of a grammar for palindromes
; over the alphabet {a, b}.


(define rule1 '(S ()))
(define rule2 '(S (a)))
(define rule3 '(S (b)))
(define rule4 '(S (a S a)))
(define rule5 '(S (b S b)))

; Here is the corresponding Context Free Grammar

(define cfg1 (list 'S (list rule1 rule2 rule3 rule4 rule5)))

; This is a grammar for a fragment of English

(define rule2-1 '(<sentence> (<subject> <verb1> <period>)))
(define rule2-2 '(<sentence> (<subject> <verb2> that <sentence>)))
(define rule2-3 '(<subject> (<article> <noun>)))
(define rule2-4 '(<subject> (<pronoun>)))
(define rule2-5 '(<noun> (woman)))
(define rule2-6 '(<noun> (man)))
(define rule2-7 '(<noun> (truth)))
(define rule2-8 '(<noun> (lizard)))
(define rule2-9 '(<pronoun> (it)))
(define rule2-10 '(<pronoun> (he)))
(define rule2-11 '(<pronoun> (she)))
(define rule2-12 '(<article> (the)))
(define rule2-13 '(<article> (a)))
(define rule2-14 '(<article> (every)))
(define rule2-15 '(<verb1> (exists)))
(define rule2-16 '(<verb1> (swims)))
(define rule2-17 '(<verb1> (pauses)))
(define rule2-18 '(<verb2> (believes)))
(define rule2-19 '(<verb2> (hopes)))
(define rule2-20 '(<verb2> (fears)))


; and the grammar:

(define cfg2
  (list '<sentence>
	(list rule2-1 rule2-2 rule2-3 rule2-4 rule2-5 rule2-6 rule2-7
	      rule2-8 rule2-9 rule2-10 rule2-11 rule2-12 rule2-13 rule2-14
	      rule2-15 rule2-16 rule2-17 rule2-18 rule2-19 rule2-20 )))

; ********************************************************************
; ** problem 7 ** (11 points)
; Write a Context Free Grammar in this representation
; to generate a set of strings that form one of the following:
; a.) A job description
; This could include job title, company description, job requirements,
; compensation and benefits. Your grammar might produce:
; Internet Software Wizard
; Exciting new start-up for yet another social network
; Three years experience in Java and Racket.
; Company equity and free Snapple.
; b.) A Blue book entry 
; This could including course number, title, and brief description, e.g.
; a string your grammar might produce would be:
; CPSC 201
; Introduction to Computer Science
; An exciting overview of the most important topic taught at Yale.
; c.) A movie description
; This could include the movie title, the cast, and brief synopsis, e.g.
; a string your grammar might produce would be:
; Rocky XXXIV
; Sylvester Stallone
; Joe Smith
; Rocky gets really old.
; 
; Your strings do not have to follow these patterns exactly,
; but the strings your grammar produces should be at least as complex. 
; You do not have to be concerned with punctuation or separating the entries into
; lines.
; You should give a number of examples of sentences
; your grammar  generates.
;
;
; Ex. (ANTH 419 Advanced Studies in Linguistics : Indubitably the most valuable and most wonderful project in Anthropology at Yale)
; Ex. (LING 211 Intro to Morph- osyntactic- osyntactic- omorph- ographic- ographic- ology : Without a doubt the most wonderful class in Linguistics at Yale)
; Ex. (PSYC 419 Intro to Psych- olexic- osyntactic- ographic- ographic- olexic- ographic- omorph- osyntactic- ology : Without a doubt the introductory project in Linguistics on the East Coast)
; name your grammar cfg-mine

(define rule3-0 '(<desc> (<line1> <line2> <line3>)))
(define rule3-1 '(<line1> (<course> <number>)))
(define rule3-2 '(<line2> (<level> <topic> <colon>)))
(define rule3-3 '(<line3> (<qualifier> <article> <adjective> <noun> in <topic> <place>)))
(define rule3-4 '(<course> (LING)))
(define rule3-5 '(<course> (PSYC)))
(define rule3-6 '(<course> (ANTH)))
(define rule3-7 '(<number> (110)))
(define rule3-8 '(<number> (211)))
(define rule3-9 '(<number> (348)))
(define rule3-10 '(<number> (419)))
(define rule3-11 '(<level> (Intro to)))
(define rule3-12 '(<level> (Advanced Studies in)))
(define rule3-13 '(<level> (Experimental)))
(define rule3-14 '(<level> (Field Study in)))
(define rule3-15 '(<topic> (Morph- <subtopic>)))
(define rule3-16 '(<topic> (Lexic- <subtopic>)))
(define rule3-17 '(<topic> (Psych- <subtopic>)))
(define rule3-18 '(<subtopic> (omorph- <subtopic>)))
(define rule3-19 '(<subtopic> (olexic- <subtopic>)))
(define rule3-20 '(<subtopic> (osyntactic- <subtopic>)))
(define rule3-21 '(<subtopic> (ographic- <subtopic>)))
(define rule3-22 '(<subtopic> (ology)))
(define rule3-22b '(<colon> (:)))
(define rule3-23 '(<qualifier> (Indubitably)))
(define rule3-24 '(<qualifier> (Without a doubt)))
(define rule3-25 '(<qualifier> (Simply)))
(define rule3-26 '(<article> (the)))
(define rule3-27 '(<adjective> (most wonderful)))
(define rule3-28 '(<adjective> (most rigorous)))
(define rule3-29 '(<adjective> (introductory)))
(define rule3-30 '(<adjective> (most valuable)))
(define rule3-31 '(<noun> (course)))
(define rule3-32 '(<noun> (class)))
(define rule3-33 '(<noun> (project)))
(define rule3-34 '(<noun> (and <adjective> <noun>)))
(define rule3-35 '(<topic> (Anthropology)))
(define rule3-36 '(<topic> (Deconstructionism)))
(define rule3-37 '(<topic> (Linguistics)))
(define rule3-38 '(<place> (at Yale)))
(define rule3-39 '(<place> (on the East Coast)))
(define rule3-40 '(<place> (this side of the galaxy)))
  
(define cfg-mine
  (list '<desc>
        (list rule3-0 rule3-1 rule3-2 rule3-3 rule3-4 rule3-5 rule3-6 rule3-7 rule3-8 rule3-9 rule3-10
                      rule3-11 rule3-12 rule3-13 rule3-14 rule3-15 rule3-16 rule3-17  rule3-18 rule3-19 rule3-20
                      rule3-21 rule3-22 rule3-22b rule3-23 rule3-24 rule3-25 rule3-26 rule3-27 rule3-28 rule3-29 rule3-30
                      rule3-31 rule3-32 rule3-33 rule3-34 rule3-35 rule3-36 rule3-37 rule3-38 rule3-39 rule3-40)))

; ********************************************************************
; ** problem  8 ** (22 points)
; Write a procedure (generate cfg)
; to generate a random Finite String
; in the language of the Context Free Grammar cfg.
; Every string in the language of the grammar should
; be a possible output of your program, and no string
; not in the language of the grammar should be a possible output.
; You may assume that every nonterminal in the grammar
; generates at least one string of terminals.
; (This is to avoid problems like (s (s (t)) (t (s))).)
; Recall the procedure (pick lst) that you wrote above.


; Examples:
; (generate cfg1) => '()
; (generate cfg1) => '(a b b b a)
; (generate cfg1) => '(a b a b b b a b a)
; (generate cfg2) => '(a lizard exists)
; (generate cfg2) => '(a truth hopes that she pauses)
; (generate cfg2) => '(he exists)
; (generate cfg2) => '(it exists)
; (generate cfg2) => '(she exists)
; (generate cfg2) => '(he fears that the man believes that it pauses)
; (generate cfg2) => '(the woman hopes that she exists)
; (generate cfg2) => '(it pauses)
; (generate cfg2) => '(she believes that every man hopes that it exists)

(define generate
  (lambda (cfg)
    (first (check-rule (first cfg) (first (pick (gen-aux (first cfg) (first (rest cfg))))) (first (rest cfg))))))

(define get-started
  (lambda (start-symb rules)
    (if (equal? start-symb (first (first rules)))
        (first rules)
        (get-started start-symb (rest rules)))))
  
(define is-non-terminal?
  (lambda (exp rules)
    (if (null? rules)
        #f
        (if (equal? exp (first (first rules)))
            #t
            (is-non-terminal? exp (rest rules))))))

(define gen-aux
  (lambda (sym rules)
    (if (null? rules)
        '()
        (if (equal? (first (first rules)) sym)
            (cons (rest (first rules)) (gen-aux sym (rest rules)))
            (gen-aux sym (rest rules))))))

(define check-rule
  (lambda (sym rule rules-list)
    (if (null? rule)
        '(())
        (if (is-non-terminal? (first rule) rules-list)
            (product (check-rule (first rule) (first (pick (gen-aux (first rule) rules-list))) rules-list) (check-rule sym (rest rule) rules-list))
            (product (list (list (first rule))) (check-rule sym (rest rule) rules-list))))))
                    

; ***************************************************************
; End of homework #6
; ********************************************************************
