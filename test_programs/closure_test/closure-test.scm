(define test
  (lambda (test-name got expected)
    (display test-name)
	(display " ... ")
    (if (eq? got expected)
	    (display "OK\n")
		(begin (display "FAILED: expected ")
		       (display expected)
			   (display " but got ")
			   (display got)
			   (newline)))))

(define add
  (lambda (x)
    (lambda (y)
	  (+ x y))))
	  
(test "argument capture: add 5 10"
      ((add 5) 10)
	  15)

(define add5 (add 5))
(test "saved closure: add5 10"
      (add5 10)
	  15)

(define add4
  (lambda (x)
    (lambda (y)
	  (lambda (z)
	    (lambda (t)
		  (+ x y z t))))))

(test "deep nesting #1: add4"
      ((((add4 2) 3) 4) 5)
	  14)

(define mult4
  (lambda (x)
    (lambda (y)
	  (define xy (* x y))
	  (lambda (z)
	    (define xyz (* xy z))
	    (lambda (t)
		  (* xyz t))))))

(test "deep nesting #2: mult4"
      ((((mult4 2) 3) 4) 5)
	  120)

(define global-var 0)
(define global-variable-visibility
  (lambda (x)
    (lambda (y)
	  (lambda (z)
	    (lambda (t)
		  (set! global-var (* x y z t)))))))

((((global-variable-visibility 2) 3) 4) 5)
(test "deep nesting #3: global-variable-visibility"
	  global-var
	  120)

(define increment-counter #f)
(define get-counter #f)
(define inc-and-get-counter #f)
(define setup-counter
  (lambda ()
    (define c -10)
    (set! increment-counter (lambda () (set! c (+ c 1))))
    (set! get-counter (lambda () c))
	(set! inc-and-get-counter
		(lambda ()
		  (increment-counter)
		  (get-counter)))
	(set! c 0)))

(setup-counter)
(test "counter #1: starting value"
      (get-counter)
	  0)

(increment-counter)
(increment-counter)
(increment-counter)

(test "counter #2: after three increments"
      (get-counter)
	  3)

(test "counter #3: increment and get"
      (inc-and-get-counter)
	  4)

(define my-plus
  (lambda (x y)
    (+ x y)))

(test "top level closure #1: my-plus"
      (my-plus 5 6)
	  11)

(set! my-plus
  (lambda (x y)
    (* x y)))

(test "top level closure #2: my-plus after set!"
      (my-plus 5 6)
	  30)

(define counter 0)
(define create-counter-incrementer
  (lambda (step)
    (lambda ()
	  (set! counter (+ counter step)))))

(define increment5 (create-counter-incrementer 5))
(test "referencing top level names from nested context #1: starting value"
      counter
      0)

(increment5)
(test "referencing top level names from nested context #2: after incrementing once"
	  counter
	  5)

(increment5)
(test "referencing top level names from nested context #3: after incrementing twice"
	  counter
	  10)

(define square (lambda (x) (* x x)))
(define pi 3.14159)
(define create-circle
  (lambda (r)
    (define radius r)
	(define get-radius (lambda () radius))
	(define get-area (lambda () (* pi (square radius))))
	(cons get-radius get-area)))

(define circle (create-circle 2))
(test "referencing top level names from nested context #4: circle radius"
      ((car circle))
	  2)

(test "referencing top level names from nested context #5: circle area"
      ((cdr circle))
	  (* pi 4))

(define capture-and-mutate-arg
  (lambda (x)
    (define get (lambda () x))
	(define set (lambda (y) (set! x y)))
	(set! x (* x 10))
	(cons get set)))

(define cpt-and-mutate (capture-and-mutate-arg 5))
(test "argument capture #1: starting value"
	  ((car cpt-and-mutate))
	  50)

((cdr cpt-and-mutate) 42)
(test "argument capture #2: after setting"
      ((car cpt-and-mutate))
	  42)

(define non-capturing-closure-1
  (lambda ()
    (lambda (x) (+ x 1))))

(test "non-capturing closures #1: at first nesting level (no other capturing lambdas in the same scope)"
      ((non-capturing-closure-1) 5)
	  6)

(define non-capturing-closure-2
  (lambda (capt)
    (define capturing (lambda () capt))
	(define non-capturing (lambda () 42))
	(cons capturing non-capturing)))

(define ncc2 (non-capturing-closure-2 99))
(test "non-capturing closures #2: at first nesting level, capturing lambda in the same scope"
      (+ ((cdr ncc2)) ((car ncc2)))
	  (+ 42 99))

(define local-non-capturing-procedures
  (lambda ()
    (define get-answer (lambda () 42))
    (define check
	  (lambda ()
	    (if (get-answer)
		    1
			-1)))
	check))


(define local-proc-call (local-non-capturing-procedures))
(test "calling a local procedure #1: named otherwise non-capturing procedures defined at the same nesting level"
      (local-proc-call)
	  1)
	  
(define create-counter
  (lambda ()
    (define c 0)
    (define increment-counter (lambda () (set! c (+ c 1))))
    (define get-counter (lambda () c))
	(define increment-and-get-counter ; makes calls through procedure objects, but could be using direct calls
	  (lambda ()
		(increment-counter)
		(get-counter)))
	increment-and-get-counter))

(define counter2 (create-counter))
(counter2)
(counter2)
(test "calling a local procedure #2: named capturing procedures defined at the same nesting level"
      (counter2)
	  3)

(define really-complex-way-of-computing-circle-area
  (lambda ()
	(define get-pi (lambda () 3.14159))
	(define get-circle-area-calculator
	  (lambda ()
	    (define area
		  (lambda (r)
		    (* (get-pi) r r)))
		  area))
	(get-circle-area-calculator)))

(define circle-area (really-complex-way-of-computing-circle-area))
(test "calling a local procedure #3: named non-capturing procedures defined at a higher nesting level"
      (circle-area 5)
	  (* (* 5 5) 3.14159))

(define some-kind-of-multiplier-thing
  (lambda (mult)
	(define get-multiplier (lambda () (* 2 mult)))
	(define multiplier-calculator
	  (lambda ()
	    (lambda (r)
		  (* (get-multiplier) r))))
	(multiplier-calculator)))

(define mult-fun (some-kind-of-multiplier-thing 5))
(test "calling a local procedure #4: named capturing procedures defined at a higher nesting level"
      (mult-fun 4)
	  40)

(define reverse
  (lambda (l)
    (define rev
	  (lambda (l acc)
	    (if (null? l)
		    acc
			(rev (cdr l) (cons (car l) acc)))))
	(rev l '())))

(define multiply
  (lambda (l n)
    (define multiply-list
	  (lambda (l acc)
	    (if (null? l)
		    acc
			(multiply-list (cdr l) (cons (* n (car l)) acc)))))
	(reverse (multiply-list l '()))))

(test "local functional loop"
      (multiply '(1 2 3 4 5) 3)
	  '(3 6 9 12 15))

