(define double
  (lambda (x)
    (+ x x)))

(define f
  (lambda (f n)
    (display (f n))))

(f double 5)
