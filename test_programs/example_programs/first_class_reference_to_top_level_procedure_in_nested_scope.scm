(define thing
  (lambda (x)
    (display "Hello, ")
	(display x)
	(newline)))

(define to-list
  (lambda l
    l))
	
(define get-thing
  (lambda () thing))

(define other-thing thing)

(other-thing "Laura") ; "Hello, Laura"

((get-thing) "Laura") ; "Hello, Laura"

(define to-lister to-list)

(display (to-lister 1 2 3 4 5)) ; (1 2 3 4 5)
(newline)

(other-thing) ; arity mismatch error