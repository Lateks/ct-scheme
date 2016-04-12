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

(define my-plus
  (lambda (x y)
    (+ x y)))

(display (apply2 my-plus 1 2))
(newline)

(display my-plus)
(newline)

(set! my-plus #f)
(display my-plus)
(newline)

(display (apply2 my-plus 1 2))

; TODO: set a new closure value for my-plus

(display (apply2 zero? 1 2))
