(define thing
  (lambda (x)
    (display "Hello, ")
	(display x)
	(newline)))

(define get-thing
  (lambda () thing))

(define other-thing thing)

(other-thing "Laura")	

((get-thing) "Laura")
