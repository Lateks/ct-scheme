(define add4
  (lambda (x)
    (lambda (y)
	  (lambda (z)
	    (lambda (t)
		  (+ x y z t))))))

(display ((((add4 2) 3) 4) 5)) ; 14
(newline)

(define mult4
  (lambda (x)
    (lambda (y)
	  (define xy (* x y))
	  (lambda (z)
	    (define xyz (* xy z))
	    (lambda (t)
		  (* xyz t))))))

(display ((((mult4 2) 3) 4) 5)) ; 120
(newline)