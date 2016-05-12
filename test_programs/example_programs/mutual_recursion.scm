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

(display (odd? 999999))
(newline)