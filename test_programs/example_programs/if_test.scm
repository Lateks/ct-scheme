(display (if #t "true" "false")) ; true
(newline)
(display (if #f "true" "false")) ; false
(newline)
(display (if (not #f) "true" "false")) ; true
(newline)
(display (if (if (< 1 2) #t #f) (if (> 2 1) 142 -1) (if (< 2 1) -1 242))) ; 142
(newline)

(if #true
    (display "true")
	(display "false")) ; true
(newline)