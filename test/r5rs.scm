(define passed 0)
(define failed 0)
;(define messages '())

	

(define expect-true
	(lambda (x)
		(if x
			(set! passed (+ 1 passed))
			(set! failed (+ 1 failed)))))
			
(define expect-false
	(lambda (x)
		(expect-true (not x))))
			
(define expect (lambda (test? a b)
	(expect-true (test? a b))))
				
(define expect-eq?
	(lambda (a b)
		(expect eq? a b)))
		

(expect-eq? 1 (if #t 1 2))
(expect-eq? 2 (if #f 1 2))
(expect-eq? 1 (if '() 1))

(expect-eq? 'yes (if (> 3 2) 'yes 'no))
(expect-eq? 'no  (if (> 2 3) 'yes 'no))
(expect-eq? 1    (if (> 3 2)
									(- 3 2)
									(+ 3 2)))
									
(expect-true 1)
(expect-true 3.1415)
(expect-true '())
(expect-true #t)
(expect-true 'symbol)
(expect-true "string")
(expect-true +)      ; a built-in function
(expect-true expect) ; a compiled function
(expect-false #f)


(display "Passed: ")
(display passed)
(display "Failed: ")
(display failed)