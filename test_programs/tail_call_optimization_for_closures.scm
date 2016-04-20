(define odd?
  (lambda (n)
    (if (zero? n)
	     #f
		 (even? (- n 1)))))

(define odd-fun? odd?)
		 
(define even?
  (lambda (n)
    (if (zero? n)
	    #t
		(odd-fun? (- n 1)))))

(define even-fun? even?)

(define param 999999)
(display "Checking whether ")
(display param)
(display " is odd: ")		
(display (odd-fun? 999999))
(newline)