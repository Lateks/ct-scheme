(define test
  (lambda (test-name test expected)
	(define perform-test
	  (lambda ()
		(define got (test))
		(if (eq? got expected)
			(display "OK\n")
			(begin (display "FAILED: expected ")
		       (display expected)
			   (display " but got ")
			   (display got)
			   (newline)))))
    (display test-name)
	(display " ... ")
	(perform-test)
))

(define sum-of-nine
  (lambda (a b c d e f g h i)
    (+ a b c d e f g h i)))

(test "parameter count > 5 #1: (sum-of-nine 1 2 3 4 5 6 7 8 9)"
	  (lambda () (sum-of-nine 1 2 3 4 5 6 7 8 9))
	  45)

(define sum-of-nine-closure sum-of-nine)

(test "parameter count > 5 #2: calling sum-of-nine as a first class procedure"
	  (lambda () (sum-of-nine-closure 1 2 3 4 5 6 7 8 9))
	  45)

(define sum-of-six-closure
  (lambda ()
    (lambda (a b c d e f) (+ a b c d e f))))

(test "parameter count > 5 #3: creating an anonymous procedure with > 5 parameters"
      (lambda () ((sum-of-six-closure) 1 2 3 4 5 6))
	  21)

(define sum-of-six-closure-with-capture
  (lambda (n)
    (lambda (a b c d e f) (+ a b c d e f n))))

(test "parameter count > 5 #4: creating a capturing anonymous procedure with > 5 parameters"
      (lambda () ((sum-of-six-closure-with-capture 10) 1 2 3 4 5 6))
	  31)

(define add
  (lambda (x)
    (lambda (y)
	  (+ x y))))
	  
(test "argument capture: add 5 10"
      (lambda () ((add 5) 10))
	  15)

(define add5 (add 5))
(test "saved closure: add5 10"
      (lambda () (add5 10))
	  15)

(define add4
  (lambda (x)
    (lambda (y)
	  (lambda (z)
	    (lambda (t)
		  (+ x y z t))))))

(test "deep nesting #1: add4"
      (lambda () ((((add4 2) 3) 4) 5))
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
      (lambda () ((((mult4 2) 3) 4) 5))
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
	  (lambda () global-var)
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
      (lambda () (get-counter))
	  0)

(increment-counter)
(increment-counter)
(increment-counter)

(test "counter #2: after three increments"
      (lambda () (get-counter))
	  3)

(test "counter #3: increment and get"
      (lambda () (inc-and-get-counter))
	  4)

(define my-plus
  (lambda (x y)
    (+ x y)))

(test "top level closure #1: my-plus"
      (lambda () (my-plus 5 6))
	  11)

(set! my-plus
  (lambda (x y)
    (* x y)))

(test "top level closure #2: my-plus after set!"
      (lambda () (my-plus 5 6))
	  30)

(define counter 0)
(define create-counter-incrementer
  (lambda (step)
    (lambda ()
	  (set! counter (+ counter step)))))

(define increment5 (create-counter-incrementer 5))
(test "referencing top level names from nested context #1: starting value"
      (lambda () counter)
      0)

(increment5)
(test "referencing top level names from nested context #2: after incrementing once"
	  (lambda () counter)
	  5)

(increment5)
(test "referencing top level names from nested context #3: after incrementing twice"
	  (lambda () counter)
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
      (lambda () ((car circle)))
	  2)

(test "referencing top level names from nested context #5: circle area"
      (lambda () ((cdr circle)))
	  (* pi 4))

(define capture-and-mutate-arg
  (lambda (x)
    (define get (lambda () x))
	(define set (lambda (y) (set! x y)))
	(set! x (* x 10))
	(cons get set)))

(define cpt-and-mutate (capture-and-mutate-arg 5))
(test "argument capture #1: starting value"
	  (lambda () ((car cpt-and-mutate)))
	  50)

((cdr cpt-and-mutate) 42)
(test "argument capture #2: after setting"
      (lambda () ((car cpt-and-mutate)))
	  42)

(define non-capturing-closure-1
  (lambda ()
    (lambda (x) (+ x 1))))

(test "non-capturing closures #1: at first nesting level (no other capturing lambdas in the same scope)"
      (lambda () ((non-capturing-closure-1) 5))
	  6)

(define non-capturing-closure-2
  (lambda (capt)
    (define capturing (lambda () capt))
	(define non-capturing (lambda () 42))
	(cons capturing non-capturing)))

(define ncc2 (non-capturing-closure-2 99))
(test "non-capturing closures #2: at first nesting level, capturing lambda in the same scope"
      (lambda () (+ ((cdr ncc2)) ((car ncc2))))
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
      (lambda () (local-proc-call))
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
      (lambda () (counter2))
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
      (lambda () (circle-area 5))
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
      (lambda () (mult-fun 4))
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
      (lambda () (multiply '(1 2 3 4 5) 3))
	  '(3 6 9 12 15))

(define make-list
  (lambda l
    (define get-first (lambda () (car l)))
	(define get-last
	  (lambda ()
	    (define last
		  (lambda (l)
		    (if (null? (cdr l))
		        (car l)
			    (last (cdr l)))))
		(last l)))
	(cons get-first get-last)))

(define lista (make-list 1 2 3 4 5 6 7 8 9 10))
(define head (car lista))
(define last (cdr lista))
(test "varargs argument capture #1"
      (lambda () (head))
	  1)
(test "varargs argument capture #2"
      (lambda () (last))
	  10)

(define add-fc
  (lambda (x)
    (define addx
	  (lambda (y)
	    (+ x y)))
	addx))

(test "first class use of local procedure name #1"
      (lambda () ((add-fc 4) 6))
	  10)

(define transform-and-double
  (lambda (f v)
    (* (f v) 2)))

(define transform
  (lambda (v1 v2)
    (define f
	  (lambda (v)
	    (/ v v2)))
	(transform-and-double f v1)))

(test "first class use of local procedure name #2"
	  (lambda () (transform 6 3))
	  4)

(define double-closure
  (lambda (x)
    (define c x)
	(define f (lambda () (set! c (+ c 1)) c))
	(cons f f)))

(define dc (double-closure 41))
(define first-result ((car dc)))
(define second-result ((cdr dc)))

(test "first class use of local procedure name #3"
      (lambda () first-result)
	  42)

(test "first class use of local procedure name #4"
	  (lambda () second-result)
	  43)

(define odd?
  (lambda (n)
    (if (zero? n)
	     #f
		 (even? (- n 1)))))

(define even?
  (lambda (n)
    (if (zero? n)
	    #t
		(odd? (- n 1)))))

(test "mutual recursion between top level procedures"
      (lambda () (odd? 9999999))
	  #true)

(define odd-fun? #f)
(define even-fun? #f)	  

(set! odd-fun?
  (lambda (n)
    (if (zero? n)
	     #f
		 (even-fun? (- n 1)))))

(set! even-fun?
  (lambda (n)
    (if (zero? n)
	    #t
		(odd-fun? (- n 1)))))

(test "mutual recursion between closures created inside the module body (main method)"
      (lambda () (odd-fun? 9999999))
	  #true)

(define is-odd
  (lambda (n) ; emulating "continuation passing style" locally
    (define odd?
	  (lambda (n cont)
        (if (zero? n)
		    #f
			(cont (- n 1) odd?))))
	(define even?
	  (lambda (n cont)
	    (if (zero? n)
		    #t
			(cont (- n 1) even?))))
	(odd? n even?)))

(test "mutual recursion between local closures passed as first class objects to procedures"
	(lambda () (is-odd 9999999))
	#true)
