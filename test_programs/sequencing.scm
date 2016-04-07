(begin (display "hello")
       (newline)
	   (display "world")
	   (newline))

(display (and #t #f #t)) ; #f
(and (display "hello\n") #f (display "world\n")) ; hello
(and (display "foo\n") #t (display "baz\n")) ; foo\nbaz
(display (and #t)) ; #t
(newline)
(display (and #f)) ; #f
(newline)

(display (begin (display "hello, ") "world\n"))

(display (or)) ; #f
(newline)
(display (and)) ; #t
(newline)
(display (or (display "hello") (display "world")))
(newline)
(display (or #f #t)) ; #t
(newline)
(display (or #t #f)) ; #t
(newline)
(display (or #f #f)) ; #f
(newline)
(display (or #t)) ; #t
(newline)
(display (or #f)) ; #f
