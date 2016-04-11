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
