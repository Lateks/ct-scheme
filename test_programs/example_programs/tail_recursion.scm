(define fact-helper
  (lambda (n acc)
	(if (zero? n)
		acc
		(fact-helper (- n 1) (* acc n)))))

(define fact
  (lambda (n)
	(fact-helper n 1)))

(display (fact 10))
(newline)

(define fact2
  (lambda (n)
	(define fact-helper
	  (lambda (n acc)
		(if (zero? n)
			acc
			(fact-helper (- n 1) (* acc n)))))
	(fact-helper n 1)))

(display (fact2 10))
(newline)
