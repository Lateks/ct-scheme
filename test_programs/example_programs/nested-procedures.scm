(define get-adder
  (lambda ()
    (lambda (x y)
	  (+ x y))))

(display ((get-adder) 10 9)) ; 19
(newline)
	  
(define adder (get-adder))

(display (adder 4 6)) ; 10
(newline)

(define add
  (lambda (x)
    (lambda (y)
	  (+ x y))))
	  
(display ((add 5) 10))
(newline)

(define add5 (add 5))
(display (add5 10))
(newline)

(define add3
  (lambda (x)
    (lambda (y)
	  (lambda (z)
	    (+ x y z)))))

(define expected (+ 1 2 3))
(define got (((add3 1) 2) 3))
(if (eq? expected got)
    (display "add3 ok\n")
	(begin (display "add3 is wrong\n")
	       (display "expected ")
		   (display expected)
		   (display " but got ")
		   (display got)
		   (newline)))

(define mutate-arg
  (lambda (x)
    (define get (lambda () x))
	;(define set (lambda (y) (set! x y)))
	(set! x (* x 10))
	get))

(display ((mutate-arg 5))) ; 50
(newline)
