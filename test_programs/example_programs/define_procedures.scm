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
			   
(println "Hello, world!") ; Hello, world!

(display (test2 5 4)) ; 9
(newline)

(display (by-value-and #t #f)) ; #f
(newline)

(display (min 5 4)) ; y is smaller\n4
(newline)
(display (min 2 3)) ; x is smaller\n2
(newline)

(define accepted-id 5)

(define get-access
  (lambda (id)
    (if (eq? id accepted-id)
	    "access granted")))

(display (get-access 1)) ; empty line (undefined)
(newline)
(display (get-access 5)) ; "access granted"

(define assignment-test
   (lambda (x y)
     (set! x y)
	 x))

(newline)
(display (assignment-test 1 2)) ; 2
(newline)
