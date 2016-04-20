(define odd?
  (lambda (n)
    (if (zero? n)
	     #f
		 (even-fun? (- n 1)))))

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

(define foo
  (lambda ()
    (display "lol\n")))

(define fooer foo)
(fooer)

(define bar
  (lambda (x y z)
    (+ x y z)))

(define barrer bar)
(display (barrer 1 2 3))
(newline)

(define lol
  (lambda l l))

(define loller lol)
(display (loller 1 2 3 4 5))
(newline)
