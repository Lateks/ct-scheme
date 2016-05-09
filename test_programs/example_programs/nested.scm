(define get-even
  (lambda ()
    (define even?
	  (lambda (n)
	    (if (zero? n)
		    #t
			(odd? (- n 1)))))
	(define odd?
	  (lambda (n)
	    (if (zero? n)
		    #f
			(even? (- n 1)))))
	even?))

(display ((get-even) 20)) ; #t
(newline)
(display ((get-even) 19)) ; #f
(newline)
