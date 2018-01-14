

; Name: Isaac Pena
; Email address: isaac.pena@yale.edu
; CPSC 201, HW #5; due November 11, 2014 11:59 pm, electronically
; using the submit command.
; Lateness penalties (5 points per day) will apply.
;
;

; Topics in this homework: mutators, queues, objects and simulation.

; We will build a simulation of a bank where customers arrive at
; random intervals, join a queue, and wait there until a teller is
; available to serve them. In one configuration, the bank will have
; a single queue, whereas in another each teller will have their own
; independent queue.

; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment (including reading):

(define hours 18)

; Problem 1 9pts
; To perform the simulation we need a pseudo-random number
; generator. See the class notes on discrete event
; simulation. 

; Define, in the style of R5RS objects, a procedure 
; make-prng that will take the 4 parameters x0 (for the seed),
; a and c (for the constants) and m for the modulus 
; to get a series of pseudorandom numbers.
; Objects to produce random numbers will be created using
; (define random-object (make-prng x a c m ))
; and will produce the next random number when given the
; command (random-object 'next!), will report the modulus
; when given the command (random-object 'modulus) and will start a new series
; when given (random-object 'reseed! new_x)

(define make-prng
  (lambda (x a c m)
    (lambda (command . args)
      (case command
         ((next!) (let ((y (modulo (+ (* a x) c) m))) (set! x y) y))
         ((modulus) m)
         ((reseed!) (set! x (car args)) #t)
         (else 'error)))))

; The procedure loopy can be used to generate lists of any length
; of random numbers for testing purposes.
(define loopy
  (lambda (n rand-obj lst)
  (if (= 0 n) lst  (loopy (- n 1) rand-obj (cons (rand-obj 'next!) lst)))))


; Note that the following define and two runs of loopy must be run in order as shown to 
; produce the results shown
;(define my-test-random (make-prng 23984 20 13 212))
;(loopy 10 my-test-random '()) => (209 137 197 41 65 45 97 89 25 149)
;(loopy 10 my-test-random '()) => (201 73 109 185 157 145 49 129 133 165)

; Note that my-test-random needs to have been defined as above, and then reseed!
; and the two runs of loopy must be run in order as shown to produce the
; results shown
;(my-test-random 'reseed! 9999234) => #t
;(loopy 10 my-test-random '()) => (69 77 141 17 85 205 105 153 113 5)
;(loopy 10 my-test-random '()) => (117 37 33 1 169 29 181 125 101 121)

; this result depends on the definition of my-test-random made earlier
;(my-test-random 'modulus) => 212

;(my-test-random 1342432) => error


; Problem 2 10pts
; For our simulation we are going to need random time intervals
; To perform the simulation we use a pseudo-random number
; to generate a time interval. We need to invert the
; cumulative distribution function given in the notes for
; discrete event simulation to generate intervals of the right frequency. Specifically,
; we will want small intervals to be common and longer 
; intervals to be less common. 
;
; Define the procedure (interval rand-obj t-ave)
; that will use a random number generator
; defined  by make-prng  and generate a series of intervals
; with an average value of t-ave. 

; Note that scaling the
; random number to be between 0 and 1 depends on the value
; of the modulus m you use for the random number generator, and that 
; random numbers that would result in attempting to 
; evaluate the log of 0 must be discarded to avoid an error.
;
; That is, you should first get a new random value x and evaluate (1- x/(m-1)). If this
; is greater than zero return -(1/t-ave)(log 1-(x/(m-1))), 
; otherwise call (interval rand-obj t-ave) again. 

(define interval  
  (lambda (rand-obj t-ave)
    (let ((m (rand-obj 'modulus)) (x (rand-obj 'next!)))
      (if (< 0 (- 1 (/ x (- m 1))))
          (* (- 0 t-ave) (log (- 1 (/ x (- m 1)))))
          (interval rand-obj t-ave)))))

; Note these tests must be run in this specific order to
; produce the stated results
;(define random-interval-test (make-prng 0 3 1 17))
;(interval random-interval-test 5) => 0.3226926056878559
;(interval random-interval-test 2) => 0.5753641449035618 
;(interval random-interval-test 7) => 11.717835035001702
;(interval random-interval-test 5) => 2.350018146228678
;(interval random-interval-test 5) => 0.6676569631226131
;(interval random-interval-test 5) => 2.876820724517809
;(interval random-interval-test 5) => 1.8734672472070535
;(interval random-interval-test 5) => 13.862943611198906
;(interval random-interval-test 5) => 6.931471805599453
;(interval random-interval-test 5) => 1.0381968238912225
;(interval random-interval-test 2) => 1.9616585060234524
;(interval random-interval-test 7) => 14.55609079175885
;(interval random-interval-test 5) => 4.13339286592234
;(interval random-interval-test 5) => 5.8157540490284045
;(interval random-interval-test 5) => 0
;(interval random-interval-test 5) => 0.3226926056878559 

; Problem 3 15pts

; We will represent queues in the simulation with queue data structures.
;
; A queue object will be initially empty and should accept the
; following commands:
;
;   'empty?    return #t if the queue is empty, #f otherwise
;   'head      return the first value in a non-empty queue
;   'enqueue!  add a value at the end of the queue
;   'dequeue!  drop the first value in a queue
;   'length    return the number of values currently in the queue
;
; In our complete simulation, when a customer joins a queue we will
; use 'enqueue! to add the value of the customer's arrival time to 
; the queue.
;
; DO NOT USE THE PROCEDURE append for enequeuing new values you will
; receive 0 points for this problem if you do!!!! Instead, you should
; arrange your data structure in such a way that new values can be
; added at the end of the queue instantly (without having to process
; each existing element, the way append does).
;
; Likewise, DO NOT USE THE PROCEDURE length
; for finding the length of the queue. The time to execute the built
; in procedure length depends on the length of the list. Make the
; command constant time by maintaining a value of length for each
; queue object. You will receive 0 points for this problem if you use
; the built in procedure length for the 'length command.

; Define the procedure (make-queue) 
; that will allow the definition of queue objects.


(define make-queue
  (lambda ()
    (let ((dummy (cons 'end '())))
      (let ((frst dummy)
            (last dummy)
            (len 0))
      (lambda (command . args)
        (case command
          ((empty?) (if (= 0 len) #t #f))
          ((head) (car frst))
          ((enqueue!) (let ((next-dum (cons 'end '())))
                        (set-car! last (car args))
                        (set-cdr! last next-dum)
                        (set! last (cdr last))
                        (set! len (+ 1 len))))
          ((dequeue!) (cond ((equal? 'end (car frst)) (set! len len)) 
                            (else (set! frst (cdr frst))
                                  (set! len (- len 1)))))
          ((length) len)))))))

;(define sample-queue (make-queue))
;(sample-queue 'length) => 0
;(sample-queue 'dequeue!)
;(sample-queue 'length) => 0
;(sample-queue 'enqueue! 5.55)
;(sample-queue 'enqueue! 6.78)
;(sample-queue 'enqueue! 8.88)
;(sample-queue 'length) => 3
;(sample-queue 'head) => 5.55
;(sample-queue 'dequeue!)
;(sample-queue 'head); => 6.78
;(sample-queue 'length) => 2
;(sample-queue 'head) => 6.78


; Problem 4 15pts
; We need three more types of objects. 
; We need a clock object,   teller objects and a next-arrival object. 
;
; A clock is initiated with the value end time.
; Define the procedure (make-clock end-time). The value end-time is the
; total length of our simulation, and we assume the clock starts at 0.
; The commands we need are:
;
;   'update! <new-val>   set the clock to <new-val>;
;   'current-time        return the clock's current time;
;   'close?              return #t if we have gotten to the end time and
;                        should close down the simulation, #f otherwise.
;
; Define the procedure (make-teller rand-obj t-ave-transaction)
; to make a teller object that keeps
; track of the time at which the teller will be done with the current
; customer and available for the next one, and that provides the commands:
;
;   'update!             increases the next available time by the result of
;                        (interval rand-obj t-ave-transaction), which
;                        represents how long the teller will be busy with
;                        the current customer;
;   'available           report when the teller will be available for a
;                        new customer.
;
; We assume that initially all tellers are available at time 0.
;
; Define the procedure (make-time-of-next-arrival rand-obj t-ave-arrival) to make a next-arrival object
; that will keep track of the time at which the next customer is expected to arrive
; and will provide the following commands:
;
;   'update!             add (interval rand-obj t-ave-arrival) to the
;                        current next arrival time;
;   'val                 inquires about the next arrival time.
;
; Initially the next arrival time  will have value 0. In our complete simulation this will mean
; that, somewhat unrealistically, there is always a customer waiting to get in as soon as the
; bank opens.
;
;
; Note that the average interval times t-ave-arrival for arrivals will be different than the average
; interval times t-ave-transaction for a teller completing a transaction. However, the same random 
; number object rand-obj  can be used for both the teller objects and the next-arrival object.

; In our complete simulation, we will need one clock object, one next-arrival object, and several
; teller objects.
       
(define make-clock
  (lambda (end)
    (let ((time 0))
     (lambda (command . args)
       (case command
         ((current-time) time)
         ((update!) (set! time (car args)))
         ((close?) (if (> end time) #f #t)))))))

;(define clock (make-clock 480))
;(clock 'current-time) => 0
;(clock 'update! 12) 
;(clock 'current-time) => 12
;(clock 'update! 467)
;(clock 'current-time) => 467
;(clock 'close?) => #f
;(clock 'update! 1)
;(clock 'current-time) => 1
;(clock 'close?) => #f
;(clock 'current-time) => 1
;(clock 'update! 500)
;(clock 'close?) => #t

(define make-teller
  (lambda (rand-obj t-ave)
    (let ((next-val 0))
        (lambda (command . args)
          (case command
            ((update!) (set! next-val (+ (car args) (interval rand-obj t-ave))))
            ((available-time) next-val))))))

;(define random-test-teller (make-prng 0 3 1 17))
;(define test-teller1 (make-teller random-test-teller 10))
;(define test-teller2 (make-teller random-test-teller 10))
;(test-teller1 'update! 0)
;(test-teller1 'available-time)  => 0.6453852113757118
;(test-teller1 'update! 10)
;(test-teller1 'available-time)=> 12.87682072451781
;(test-teller1 'update! 200)
;(test-teller1 'available-time) => 216.7397643357167
;(test-teller2 'available-time) => 0


(define make-time-of-next-arrival
  (lambda (rand-obj t-ave)
    (let ((value 0))
      (lambda (command)
        (case command
          ((val) value)
          ((update!) (set! value (+ value (interval rand-obj t-ave)))))))))

;(define random-test-arrival (make-prng 0 3 1 17))
;(define time-of-next-arrival (make-time-of-next-arrival random-test-arrival 3))
;(time-of-next-arrival 'val) => 0
;(time-of-next-arrival 'update!)
;(time-of-next-arrival 'val) => 0.19361556341271352
;(time-of-next-arrival 'update!)
;(time-of-next-arrival 'val) => 1.0566617807680563


; Problem 5 10pts
; In order to study multiple queues and multiple tellers, we need to generate
; a list of queues and a list of tellers. For the list of queues we need to be able
; to find the shortest queue and add on to the back of that queue.  


; Define the procedure (make-queue-list n) to generate the queue list, where n is the length of the list.
;
; Define the procedure (get-the-shortest-queue queue-list) that finds which queue in the list
; is shortest, and returns a list that consists of the index of the list
; and its length. If there is a tie for the shortest queue, the queue with the highest index is chosen.
;
; Define the procedure (add-to-the-queue-list queue-list value) that enqueues value to
; the end of the shortest queue in the list
;
; Define the procedure (make-teller-list n rand-obj t-ave) to make a teller list. It is assumed that the
; tellers all have the same average transaction length, and have the transaction lengths determined using
; the same random number object.


(define make-queue-list
  (lambda (n)
    (if (= n 1)
        (cons (make-queue) '())
        (cons (make-queue) (make-queue-list (- n 1))))))



;(define testqueue-list (make-queue-list 10))
;(length  testqueue-list) => 10
;((list-ref testqueue-list 0) 'length) => 0
;((list-ref testqueue-list 9) 'length) => 0

(define get-the-shortest-queue
  (lambda (queue-list)
    (gstq-aux queue-list 0 0)))

(define gstq-aux
  (lambda (queue-list shortest-ind current-ind)
    (if (= current-ind (length queue-list))
        (list shortest-ind ((list-ref queue-list shortest-ind) 'length))
        (if (<= ((list-ref queue-list current-ind) 'length) ((list-ref queue-list shortest-ind) 'length))
            (gstq-aux queue-list current-ind (+ 1 current-ind))
            (gstq-aux queue-list shortest-ind (+ 1 current-ind))))))

;(define tqueue-list2 (make-queue-list 3))
;((list-ref tqueue-list2 0) 'enqueue! .6)
;((list-ref tqueue-list2 0) 'enqueue! .7)
;((list-ref tqueue-list2 1) 'enqueue! .3)
;((list-ref tqueue-list2 1) 'enqueue! .3)
;((list-ref tqueue-list2 2) 'enqueue! .4)
;(get-the-shortest-queue tqueue-list2) => (2 1)


;((list-ref tqueue-list2 1) 'dequeue!)
;((list-ref tqueue-list2 0) 'length) => 2
;((list-ref tqueue-list2 1) 'length) => 1
;((list-ref tqueue-list2 2) 'length)  => 1
;(get-the-shortest-queue tqueue-list2 ) => (2 1)
;((list-ref tqueue-list2 1) 'dequeue!)
;(get-the-shortest-queue tqueue-list2 ) => (1 0)


(define add-to-the-queue-list
  (lambda (queue-list element)
    ((list-ref queue-list (car (get-the-shortest-queue queue-list))) 'enqueue! element)))


;((list-ref tqueue-list2 0) 'length); => 2
;((list-ref tqueue-list2 1) 'length); => 0 
;((list-ref tqueue-list2 2) 'length); => 1
;(add-to-the-queue-list tqueue-list2 .77)
;((list-ref tqueue-list2 1) 'head); => .77
;(add-to-the-queue-list tqueue-list2 .98)
;((list-ref tqueue-list2 0) 'length); => 2
;((list-ref tqueue-list2 1) 'length); => 1 
;((list-ref tqueue-list2 2) 'length); => 2
;(add-to-the-queue-list tqueue-list2 .99)
;((list-ref tqueue-list2 0) 'length); => 2
;((list-ref tqueue-list2 1) 'length); => 2
;((list-ref tqueue-list2 2) 'length); => 2


(define make-teller-list
  (lambda (n rand-obj t-ave)
    (if (= n 1)
        (cons (make-teller rand-obj t-ave) '())
        (cons (make-teller rand-obj t-ave) (make-teller-list (- n 1) rand-obj t-ave)))))

;(define random-test-teller-list (make-prng 0 3 1 17))
;(define teller-list3 (make-teller-list 10 random-test-teller-list 5))
;((list-ref teller-list3 3) 'update! 100)
;((list-ref teller-list3 3) 'available-time) => 100.32269260568786
;((list-ref teller-list3 2) 'update! 100)
;((list-ref teller-list3 2) 'available-time) => 101.4384103622589


; Problem 6 10pts
; To analyze the results of our simulation, we want to keep results that we can
; use to compute statistics. We will make a statistics object with (make-statistics). The
; statistics object will be a list for which we can issue the commands:
;
;   'empty?       to see if there are any values saved yet;
;   'add! <val>   to add the value <val> to the list;
;   'get-list     to obtain the full list of values.
;
; The statistics we will keep track of will be about how long customers have to wait before getting
; to a teller. As each customer leaves a queue we will add the difference between the time they
; were served and the time they joined the queue to the statistics list. The first value on the list
; be how long the last customer getting served last had to wait in the queue. The last value on the 
; list will be how long the first customer waited. Since initially the tellers are all available, the
; first customer will always have a wait time of 0.

; At the end of our complete simulation we will want to get the maximum time 
; from our list, and the average time.

; Define the procedure (get-the-longest-time list) that returns the largest
; in a non-empty flat list of numbers.

; Define the procedure (get-the-average-time list) that returns the average
; in a non-empty flat list of numbers.


(define make-statistics
  (lambda ()
    (let ((stack '()))
      (lambda (command . args)
        (case command
          ((add!) (set! stack (cons (car args) stack)))
          ((empty?) (if (null? stack) #t #f))
          ((get-list) stack))))))


;(define statistics (make-statistics))
;(statistics 'empty?)  => #t
;(statistics 'add! 12)
;(statistics 'add! 15)
;(statistics 'add! 11)

;(statistics 'get-list) => (11 15 12)
;(statistics 'empty?) => #f

(define get-the-longest-time
  (lambda (stat-list)
    (gtlt-aux stat-list 0)))

(define gtlt-aux
  (lambda (stat-list highest-num)
    (if (null? stat-list)
        highest-num
        (cond ((> (car stat-list) highest-num) (set! highest-num (car stat-list))
                                               (gtlt-aux (cdr stat-list) highest-num))
              (else (gtlt-aux (cdr stat-list) highest-num))))))

; When we make our list how long it took to wait on each customer, we can
; we use a procedure (get-the-longest-time list) that returns the largest
; in a series of numbers to find the longest wait time, and we use the
; procedure (get-the-average-time list) to find the average time people
; had to wait.


;(get-the-longest-time '(1 2 3 88 6 5 44)) => 88
;(get-the-longest-time '(0.5 2.0 .30324127))  => 2.0

(define get-the-average-time
  (lambda (stat-list)
    (/ (gtat-aux stat-list 0) (length stat-list))))

(define gtat-aux
  (lambda (stat-list count)
    (if (null? stat-list)
        count
        (gtat-aux (cdr stat-list) (+ (car stat-list) count)))))



;(get-the-average-time '(1 2 3 4 5 6 7)) => 4
;(get-the-average-time '(0.5 3 .24 .25 .252398274)) => 0.8484796548


; Problem 7 10pts
; 
 
; We need  the list  (index  available-time) for the teller which has the next 
; available time and who receives customers from a  currently non-empty queue. We need to
; specify a simulation parameter "choice" that is 'single if we are considering a single queue
; system, or that is 'multi if there is one queue for each teller.
;
; customer comes in, when they get added to the queue what you're adding is the clock time when they arrive. 
;
; Define the procedure (get-the-next-teller choice teller-list queue-list) that
; returns the the list (index available-time)

(define all-empty?
  (lambda (queue-list)
    (if (null? queue-list)
        #t
        (if (= 0 ((car queue-list) 'length))
            (all-empty? (cdr queue-list))
            #f))))

(define lowest-time
  (lambda (teller-list queue-list lowest n p)
    (if (null? teller-list)
        (list p lowest)
        (if (equal? ((car queue-list) 'head) 'end)
            (lowest-time (cdr teller-list) (cdr queue-list) ((car (cdr teller-list)) 'available-time) (+ n 1) p)
            (if (<= ((car teller-list) 'available-time) lowest)
                (lowest-time (cdr teller-list) (cdr queue-list) ((car teller-list) 'available-time) (+ n 1) n)
                (lowest-time (cdr teller-list) (cdr queue-list) lowest (+ n 1) p))))))

(define lowest-time-sing
  (lambda (teller-list lowest n p)
    (if (null? teller-list)
        (list p lowest)
        (if (<= ((car teller-list) 'available-time) lowest)
            (lowest-time-sing (cdr teller-list) ((car teller-list) 'available-time) (+ n 1) n)
            (lowest-time-sing (cdr teller-list) lowest (+ n 1) p)))))
           
           
           
(define get-the-next-teller
  (lambda (choice teller-list queue-list)
    (cond ((all-empty? queue-list) '())
          ((equal? choice 'multi) (lowest-time teller-list queue-list ((car teller-list) 'available-time) 0 0))
          ((equal? choice 'single) (lowest-time-sing teller-list ((car teller-list) 'available-time) 0 0)))))


;(define random-test-multi (make-prng 0 3 1 17))
;(define queue-list-multi (make-queue-list 3))
;(define teller-list-multi (make-teller-list 3 random-test-multi 1))

; If the queues are empty, the next teller returns
; an empty list

;(get-the-next-teller 'multi teller-list-multi queue-list-multi) => ()

; Test the case queues are non-empty
;(add-to-the-queue-list queue-list-multi 1)
;(add-to-the-queue-list queue-list-multi 1.1)
;(add-to-the-queue-list queue-list-multi 1.15)
;(add-to-the-queue-list queue-list-multi 1.15)
;(add-to-the-queue-list queue-list-multi 1.15)
;(add-to-the-queue-list queue-list-multi 1.15)

; If all tellers availability  the same, highest index chosen
;(get-the-next-teller 'multi teller-list-multi queue-list-multi ) => (2  0)

; Otherwise, choose next available
;((list-ref teller-list-multi 0) 'update! 0)
;((list-ref teller-list-multi 1) 'update! 10)
;((list-ref teller-list-multi 2) 'update! 10)
;(get-the-next-teller 'multi teller-list-multi queue-list-multi ) => (0  0.06453852113757118)
;((list-ref teller-list-multi 0) 'update! 1000)
;(get-the-next-teller 'multi teller-list-multi queue-list-multi ) => (1  10.287682072451782)
; If teller has an empty queue, that teller is not chosen
;((list-ref queue-list-multi 1) 'dequeue!)
;((list-ref queue-list-multi 1) 'dequeue!)
;(get-the-next-teller 'multi teller-list-multi queue-list-multi ) => (2 11.67397643357167)

; Test of the single queue case
;(define random-test-single (make-prng 0 3 1 17))
;(define queue-list-single (make-queue-list 1))
;(define teller-list-single (make-teller-list 3 random-test-single 1))
;(add-to-the-queue-list queue-list-single 1)
;(add-to-the-queue-list queue-list-single 1.1)
;(add-to-the-queue-list queue-list-single 1.15)
;(add-to-the-queue-list queue-list-single 1.15)
;(add-to-the-queue-list queue-list-single 1.15)
;(add-to-the-queue-list queue-list-single 1.15)
;((list-ref teller-list-single 0) 'update! 0)
;((list-ref teller-list-single 1) 'update! 100)
;((list-ref teller-list-single 2) 'update! 10)
;(get-the-next-teller 'single teller-list-single queue-list-single ) => (0  0.06453852113757118)
;((list-ref teller-list-single 0) 'update! 1000)
;(get-the-next-teller 'single teller-list-single queue-list-single ) => (2  11.67397643357167)

; Problem 8 10pts
; Define a procedure  (next-event choice clock next-arrival teller-list queue-list statistics)
; that will allow the given procedure simulate to run.
;
; The arguments of simulate are choice ('multi or 'single queue case), the number
; of tellers n, the average time between arrivals t-arr, the average time a teller
; takes to wait on a customer t-tell, the end time end, and a random object generator
; rand-obj.
; 
; The procedure simulate generates the objects needed, and calls next-event
; with the choice argument and the objects as arguments.

; 


(define simulate
  (lambda (choice n t-arr t-tell  end rand-obj)
    (let ((clock (make-clock end))
          (next-arrival (make-time-of-next-arrival rand-obj t-arr))
          (teller-list (make-teller-list n rand-obj t-tell))
          (queue-list (make-queue-list n))
          (statistics (make-statistics)))
      (next-event choice clock next-arrival teller-list queue-list statistics))))


; Each time it is called next-event checks to see if the clock has reached the time to close.
; If it isn't time to close, the next-event finds whether an arrival or serving a customer
; will be the next event, and updates the objects appropriately

; Use this tie-breaker: if at some time the "next arrival time" is the same as the 
; "next available teller" time,  choose the teller event. 
;
; When the end of the simulation is reached next-event will return a list of three items.
; The first list item  is a list of the waiting times for the customers, and the second
; and third times are the longest and  average waiting times.  This list of waiting times for
; the customers is found by enqueueing the value of the current time when a customer joins
; a queue, and then putting the difference between when the customer is dequeued and when the
; customer was enqueued in the statistics list.

; You will notice some odd initial behavior, because customers choose the shortest queue with the highest
; queue index even if that teller is busy. Customers can not change queues once they have chosen a queue.

(define next-event
  (lambda (choice clock next-arrival teller-list queue-list statistics)
    (cond ((clock 'close?) (list (statistics 'get-list) (get-the-longest-time (statistics 'get-list)) (get-the-average-time (statistics 'get-list))))
          (else (case choice 
              ((single) (cond ((equal? (get-the-next-teller choice teller-list queue-list) '()) (clock 'update! (next-arrival 'val))
                                                                                                ((car queue-list) 'enqueue! (next-arrival 'val))
                                                                                                (next-arrival 'update!)
                                                                                                (next-event choice clock next-arrival teller-list queue-list statistics))
                    ((< (next-arrival 'val) (cadr (get-the-next-teller choice teller-list queue-list))) (clock 'update! (next-arrival 'val))
                                                                                                        ((car queue-list) 'enqueue! (next-arrival 'val))
                                                                                                        (next-arrival 'update!)       
                                                                                                        (next-event choice clock next-arrival teller-list queue-list statistics))
                    ((>= (next-arrival 'val) (cadr (get-the-next-teller choice teller-list queue-list))) (let ((this-teller-ind (car (get-the-next-teller choice teller-list queue-list)))
                                                                                                               (this-teller-av-time (cadr (get-the-next-teller choice teller-list queue-list))))
                                                                                                           (if (< (clock 'current-time) this-teller-av-time)
                                                                                                            (clock 'update! this-teller-av-time)
                                                                                                            (clock 'update! (clock 'current-time)))
                                                                                                           (statistics 'add! (- (clock 'current-time) ((car queue-list) 'head)))
                                                                                                           ((car queue-list) 'dequeue!)
                                                                                                           ((list-ref teller-list this-teller-ind) 'update! (clock 'current-time))
                                                                                                           (next-event choice clock next-arrival teller-list queue-list statistics)))))
              ((multi) (cond ((equal? (get-the-next-teller choice teller-list queue-list) '()) (clock 'update! (next-arrival 'val))
                                                                                               (add-to-the-queue-list queue-list (next-arrival 'val))
                                                                                               (next-arrival 'update!)
                                                                                               (next-event choice clock next-arrival teller-list queue-list statistics))
                    ((< (next-arrival 'val) (cadr (get-the-next-teller choice teller-list queue-list))) (clock 'update! (next-arrival 'val))
                                                                                                        (add-to-the-queue-list queue-list (next-arrival 'val))
                                                                                                        (next-arrival 'update!)
                                                                                                        (next-event choice clock next-arrival teller-list queue-list statistics))
                    ((>= (next-arrival 'val) (cadr (get-the-next-teller choice teller-list queue-list))) (let ((this-teller-ind (car (get-the-next-teller choice teller-list queue-list)))
                                                                                                               (this-teller-av-time (cadr (get-the-next-teller choice teller-list queue-list))))
                                                                                                           (if (< (clock 'current-time) this-teller-av-time)
                                                                                                               (clock 'update! this-teller-av-time)
                                                                                                               (clock 'update! (clock 'current-time)))
                                                                                                           (statistics 'add! (- (clock 'current-time) ((list-ref queue-list (car (get-the-next-teller choice teller-list queue-list))) 'head)))
                                                                                                           ((list-ref queue-list (car (get-the-next-teller choice teller-list queue-list))) 'dequeue!)
                                                                                                           ((list-ref teller-list this-teller-ind) 'update! (clock 'current-time))
                                                                                                           (next-event choice clock next-arrival teller-list queue-list statistics))))))))))
        
            


;(define random-test-multi-full1 (make-prng 0 3 1 17))
;(simulate 'multi 2 3 9  60 random-test-multi-full1)
; => ((22.437394979285706 
;  27.577696562676092
;  34.907840626587046
;  26.01606058331417
;  21.432678813133403
;  15.278866818993983
;  24.430238338723697
;  9.954542126429018
;  8.204545182770802
;  0.0
;  1.6036264511499203
;  0.0
; 2.3955230886533143
;  0)
; 34.907840626587046
; 13.874215255122653)

;(define random-test-multi-full2 (make-prng 23984 20 13 212))
;(simulate 'multi 2 3 9  60 random-test-multi-full2)
;=> ((26.48563561224325
;  32.15535900617654
;  19.493940403281485
;  22.19240993909881
;  35.863056493506626
;  20.08526084555819
;  5.520802055546898
;  0.0
;  3.897425899505709
;  0.839782732529053
;  0.0
;  0.0
;  0)
; 35.863056493506626
; 12.810282537495889)


;(define random-test-single-full1 (make-prng 0 3 1 17))
;(simulate 'single 2 3 9  60 random-test-single-full1)
;=>
;((26.214763729578365
;  27.454238245190176
;  28.246134882693575
;  23.041392847134805
;  21.86268950480142
;  13.035226227695883
;  14.655924374108725
;  16.72784730943895
;  1.363473977578785
; 3.653602777934257
;  0.0
;  0.0
;  0.0
;  0)
; 28.246134882693575
; 12.589663848296782)

;(define random-test-single-full2 (make-prng 23984 20 13 212))
;(simulate 'single 2 3 9  60 random-test-single-full2)
;=>    
;((25.81842897503642
;  23.006077318266605
;  22.217092976886708
;  25.549715467907436
;  28.121779675204998
;  23.63333968271141
;  7.62490198225559
;  3.238208432138233
;  2.5296357748697247
;  2.594652570551
;  0.0
;  0.0
;  0)
; 28.121779675204998
; 12.641064065832932)

; Problem 9 10pts
; Submit the answers to the following questions in a text file called discussion5.txt
; i.e. use /c/cs201/bin/submit 5 discussion5.txt
;
; Describe the performance of the one versus multiple queue approaches for
; 1.) a fixed average arrival rate and increasing average teller time for 5 tellers
; 2.) a fixed average arrival rate and average teller time for increasing numbers of tellers
; Include sample test results in your discussion.
;
; One of the advantages of single queue is fairness. Everyone is served in the order
; they arrive. Change the statistics gathered and reported to produce a measure of how
; fair or unfair the multi queue approach is. Give an example of your output.
;
; Even if your simulation doesn't work, you can get partial credit for this 
; problem by describing methods to compare the performance of the single and
; multiple queue approaches.

