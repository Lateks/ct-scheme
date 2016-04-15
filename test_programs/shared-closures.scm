(define test
  (lambda (x)
    (list (lambda () (+ x 1))
		  (lambda (y) (+ x y))
		  (lambda (y) y))))

(define test-fns (test 41))
(display ((car test-fns))) ; 42
(newline)
(display ((car (cdr test-fns)) 5)) ; 46
(newline)
(display ((car (cdr (cdr test-fns))) 5)) ; 5
(newline)

;(define increment #f)
;(define get #f)
;(define inc-and-get #f)
;(define setup
;  (lambda ()
;    (define c 0)
;    (set! increment (lambda () (set! c (+ c 1))))
;	(set! get (lambda () c))
;	;(set! inc-and-get (lambda () (increment) (get))))
;	#t)

;(setup)
;(display (get))
;(newline)
;(increment)
;(display (get))
;(newline)

; (define counter
;   (lambda ()
;     (define c 0)
;     (lambda ()
;       (set! (+ c 1))
;       c)))
;
; (define increment-counter (counter))
; (display (increment-counter))
; (newline)
; (display (increment-counter))
; (newline)
