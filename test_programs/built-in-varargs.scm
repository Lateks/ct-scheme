(define apply
  (lambda (f x)
    (f x)))

(define plus +)

(display (apply plus 1000 8000))
(newline)
