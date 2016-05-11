(display display) ; displays procedure
(newline)
(display +) ; displays procedure
(newline)

(define apply
  (lambda (f x)
    (f x)))

(display (apply zero? 0)) ; #t
(newline)

(define apply2
  (lambda (f x y)
    (f x y)))

(display (apply2 cons 1 2)) ; (1 . 2)
(newline)

(display (apply2 + 1 2)) ; 3
(newline)

(define plus +)
(display (plus 1 2 3 4 5 6 7)) ; 28
(newline)

(display (apply2 plus 1000 8000)) ; 9000
(newline)

(define say-hello
  (lambda (x)
    (display "Hello, ")
	(display x)
	(display "!\n")))

(say-hello "World") ; Hello, World!
(apply say-hello "Laura") ; Hello, Laura!

(define get-plus
  (lambda ()
    plus))

(display (get-plus)) ; displays procedure
(newline)

(display ((get-plus) 25 17)) ; 42
(newline)

(define my-plus
  (lambda (x y)
    (+ x y)))

(display (my-plus 5 9)) ; 14
(newline)

(display (apply2 my-plus 1 2)) ; 3
(newline)

(display my-plus) ; displays procedure
(newline)

(set! my-plus #f)
(display my-plus) ; #f
(newline)

(set! my-plus
  (lambda (x y)
    (display "my-plus procedure was set!\n")
	(display "got the following parameters:\n")
	(display x)
	(newline)
	(display y)
	(newline)))

(my-plus 5 6) ; my-plus procedure was set!\ngot the following parameters:\n5\n6\n
(display my-plus) ; displays #<procedure:anonymous1>, should it be something else? (e.g. $<procedure:my-plus> or $<procedure:my-plus2>)
(newline)

(display (apply2 zero? 1 2)) ; arity mismatch
