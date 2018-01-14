#lang racket
; ********************************************************
; CS 201a HW #0  DUE Monday 9/08/14, 11:59 pm
; ** using the Zoo submit system **

; ********************************************************
; Name: Isaac Pena
; Email address: isaac.pena@yale.edu
; ********************************************************

; This file may be loaded into DrRacket.  
; Lines beginning with semicolons are Racket comments.

; Homework #0 will be worth 20 points -- other homeworks will be 
; worth 100 points.

; One purpose of homework #0 is to make sure you can use the 
; submit system on the Zoo.  You will receive no credit for this 
; assignment unless you successfully use the submit system to submit it.

; You will be submitting two files for homework #0.
; Please name them (with correct case and extension):

; hw0.rkt (for the Racket definitions and procedures)
; response.txt (for the reading response)

; ********************************************************
; ** problem 0 ** (1 easy point) 
; replace the number 0 in this definition to
; indicate how many hours you spent doing this assignment.
; Round to the nearest integer.

(define hours 1)

; ********************************************************
; ** problem 1 ** (5 points)

; Write a procedure (euro-to-dollar num-euros exchange-rate)
; that takes as arguments two numbers representing
; euros -- total quantity of euros to be converted
; exchange-rate -- the value of one euro in dollars
; where you may assume that euros is non-negative
; and exchange rate is positive.

; The procedure should return the dollar value of num-euros.

; Examples

; (euro-to-dollar 100 1.57) => 157.0
; (euro-to-dollar 0 2) => 0
; (euro-to-dollar 5.8 0.75) => 4.35

; ********************************************************

(define (euro-to-dollar num-euros exchange-rate)
  (* num-euros exchange-rate))

; ********************************************************
; ** problem 2 ** (4 points)

; Write a procedure (hue) that takes no arguments and
; returns a *string* indicating a color.

; Example (yours will likely be different)

; (hue) => "blue"
; ********************************************************

(define (hue)
  "fuschia")

; ********************************************************
; ** problem 3 ** (10 points)

; For this problem, you will find one article (of 2 pages or more) in
; the magazine "Communications of the ACM" (CACM), in the June, July
; or August 2014 issues. CACM is available online through Yale
; Library's subscription (i.e. you can find it in orbis.yale.edu) 
; Read the article, and answer the following 
; three questions:

;  a. What did you know about the topic prior to reading the article?
;  b. What did you learn from reading the article?
;  c. What more would you like to know about the topic?

; Your answer should be *at least* 100 words and  *at most* 400 words, as counted by the 
; wc utility on the Zoo, saved in .txt format, and submitted as
; the file response.txt for assignment 0.  Please include your name 
; and email address in the file.  This problem will be graded
; done (10 points) or not done (0 points) and will have no feedback.
; I will read your responses as part of getting a better understanding
; of you and your interests.

; Note that if you prepare your answer in a word-processing program, 
; you'll probably need to save it as (plain) Text (.txt) to get the
; correct format.

; ********************************************************
; ********  end of homework #0
; ********************************************************
