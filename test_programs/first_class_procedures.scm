(display display)
(newline)
(display +)
(newline)

(define apply
  (lambda (f x)
    (f x)))

(display (apply zero? 0)) ; #t
(newline)

(define apply2
  (lambda (f x y)
    (f x y)))

(display (apply2 cons 1 2))
(newline)

(display (apply2 + 1 2))
(newline)

(define plus +)
(display (plus 1 2 3 4 5 6 7))
(newline)

(display (apply2 plus 1000 8000))
(newline)

(define say-hello
  (lambda (x)
    (display "Hello, ")
	(display x)
	(display "!\n")))

(say-hello "World")
(apply say-hello "Laura")

(define get-plus
  (lambda ()
    plus))

(display (get-plus))
(newline)

(display ((get-plus) 25 17))
(newline)

(define my-plus
  (lambda (x y)
    (+ x y)))

(display (my-plus 5 9))
(newline)

(display (apply2 my-plus 1 2))
(newline)

(display my-plus)
(newline)

(set! my-plus #f)
(display my-plus)
(newline)

; (display (apply2 my-plus 1 2))

; TODO: set a new closure value for my-plus
;(set! my-plus
;  (lambda (x y)
;    (plus x y)))

(display (apply2 zero? 1 2))
