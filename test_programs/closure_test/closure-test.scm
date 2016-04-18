(define test
  (lambda (test-name got expected)
    (display test-name)
	(display " ... ")
    (if (eq? got expected)
	    (display "ok\n")
		(begin (display "fail: expected ")
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

(define increment-counter #f)
(define get-counter #f)
(define inc-and-get-counter #f)
(define setup-counter
  (lambda ()
    (define c -10)
    (set! increment-counter (lambda () (set! c (+ c 1))))
    (set! get-counter (lambda () c))
	;(set! inc-and-get-counter (lambda () (increment) (get)))
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
