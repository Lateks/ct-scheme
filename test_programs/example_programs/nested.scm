(define is-even?
  (lambda (n)
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
	(even? n)))

(display (is-even? 20)) ; #t
(newline)
(display (is-even? 19)) ; #f
(newline)
