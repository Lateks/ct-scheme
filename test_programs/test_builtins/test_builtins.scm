(define test
  (lambda (test-name test expected)
	(define perform-test
	  (lambda ()
		(define got (test))
		(if (eq? got expected)
			(display "OK\n")
			(begin (display "FAILED: expected ")
		       (display expected)
			   (display " but got ")
			   (display got)
			   (newline)))))
    (display test-name)
	(display " ... ")
	(perform-test)
))

(test "arithmetic #1"
      (lambda () (+ 1 2 3))
      6)

(test "arithmetic #2"
      (lambda () (- 1))
      -1)

(test "arithmetic #3"
      (lambda () (- 1 2 3))
      -4)

(test "arithmetic #4"
      (lambda () (+ 5 (- 7 4)))
      8)

(test "arithmetic #5"
      (lambda () (/ (+ 5 3) (- 3 1)))
      4)

(test "arithmetic #6"
      (lambda () (* 2 4))
      8)

(test "pairs #1"
      (lambda () (car '(1 2 3)))
	  1)

(test "pairs #2"
      (lambda () (cdr '(1 2 3)))
	  '(2 3))

(define list-one-two (cons 1 (cons 2 '())))
(define pair-one-two (cons 1 2))

(test "pairs #3"
      (lambda () list-one-two)
	  '(1 2))

(test "pairs #4"
      (lambda () (eq? list-one-two pair-one-two))
	  #false)

(test "pairs #5"
      (lambda () (cdr pair-one-two))
	  2)

(test "pairs #6"
      (lambda () (cdr (cons 1 (cons "foo" "bar"))))
	  (cons "foo" "bar"))

(test "comparison #1" (lambda () (< 1 2)) #t)
(test "comparison #2" (lambda () (> 1 2)) #f)
(test "comparison #3" (lambda () (< 1 1)) #f)
(test "comparison #4" (lambda () (< 1 2 3)) #t)
(test "comparison #5" (lambda () (< 1 2 1)) #f)
(test "comparison #6" (lambda () (> 3 2 1)) #t)
(test "comparison #7" (lambda () (> 3 2 3)) #f)

(test "if #1" (lambda () (if #t "true" "false")) "true")
(test "if #2" (lambda () (if #f "true" "false")) "false")
(test "if #3" (lambda () (if (not #f) "true" "false")) "true")
(test "if #4" (lambda () (if (if (< 1 2) #t #f) (if (> 2 1) 142 -1) (if (< 2 1) -1 242))) 142)

(test "list #1" (lambda () (list)) '())
(test "list #2" (lambda () (list 1 2 3)) '(1 2 3))
(test "list #3" (lambda () (list 1 2 (list "foo" "bar" "baz"))) '(1 2 ("foo" "bar" "baz")))

(test "map #1" (lambda () (map (lambda (x) (+ x x)) '(1 2 3))) '(2 4 6))
(test "map #2" (lambda () (map (lambda (x y) (+ x y)) '(1 2 3) '(2 4 6))) '(3 6 9))

(test "null? #1" (lambda () (null? '())) #t)
(test "null? #2" (lambda () (null? (list))) #t)
(test "null? #3" (lambda () (null? '(1 2 3))) #f)
(test "null? #4" (lambda () (null? (list 1))) #f)
(test "null? #5" (lambda () (null? "")) #f)
(test "null? #6" (lambda () (null? "Hello")) #f)

(test "not #1" (lambda () (not 1)) #f)
(test "not #2" (lambda () (not "foo")) #f)
(test "not #3" (lambda () (not #t)) #f)
(test "not #4" (lambda () (not #f)) #t)

(test "zero? #1" (lambda () (zero? 42)) #f)
(test "zero? #2" (lambda () (zero? 0)) #t)
(test "zero? #3" (lambda () (zero? 0.0)) #t)

(define test-begin
  (lambda ()
    (define x 1)
	(begin (set! x 42)
		   (set! x (* x x))
		   x)))

(test "begin" test-begin (* 42 42))
		   
(test "and #1" (lambda () (and #t #f #t)) #f)

(define test-and
  (lambda (bool a b)
    (lambda ()
      (define x 0)
	  (define y 0)
	  (and (set! x a) bool (set! y b))
	  (cons x y))))

(test "and #2" (test-and #f 5 42) (cons 5 0))	
(test "and #3" (test-and #t 5 42) (cons 5 42))

(test "and #4" (lambda () (and #t)) #t)
(test "and #5" (lambda () (and #f)) #f)
(test "and #6" (lambda () (and)) #t)

(test "or #1" (lambda () (or)) #f)
(test "or #2" (lambda () (or #f #t)) #t)
(test "or #3" (lambda () (or #t #f)) #t)
(test "or #4" (lambda () (or #f #f)) #f)
(test "or #5" (lambda () (or #t)) #t)
(test "or #6" (lambda () (or #f)) #f)
