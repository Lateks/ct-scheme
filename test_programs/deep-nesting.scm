(define add4
  (lambda (x)
    (lambda (y)
	  (lambda (z)
	    (lambda (t)
		  (+ x y z t))))))

(display ((((add4 2) 3) 4) 5)) ; 14

