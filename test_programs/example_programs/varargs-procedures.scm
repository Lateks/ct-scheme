(define my-list
  (lambda l l))

(display (my-list 1 2 3))
(newline)

(define my-list-copy my-list)

(display (my-list-copy 1 2 3 4 5 6 7 8 9 10))
(newline)

(display (+ 1 2 3))
(newline)

(display (+))
(newline)

(define print-list
  (lambda (l)
    (if (null? l)
	    (newline)
		(begin (display (car l))
		       (display " ")
			   (print-list (cdr l))))))

(define print-values
  (lambda l
    (print-list l)))

(print-values 1 2 #t #f 3.14159 "hello")
