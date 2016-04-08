(define println
  (lambda (x)
    (display x)
	(newline)))

(define test2
  (lambda (x y)
    (define temp (+ x y))
	temp))

(define by-value-and
  (lambda (v1 v2)
    (and v1 v2)))
	
(define min
  (lambda (x y)
    (if (< x y)
	    (begin (display "x is smaller\n")
			   x)
		(begin (display "y is smaller\n")
			   y))))

(println "Hello, world!")

(display (test2 5 4))
(newline)

(display (by-value-and #t #f))
(newline)

(display (min 5 4))
(newline)
(display (min 2 3))
(newline)