(define errors 0)
(define pass   0)

(define (report-error expected actual)
  (set! errors (+ 1 errors))
  (display "ERROR: Expected ")
  (display expected)
  (display " but received ")
  (display actual)
  (newline))

(define (passed actual)
  (set! pass (+ 1 pass))
  (display "PASS: ")
  (display actual)
  (newline))
  
(define (assert-equal expected actual)
  (if (equal? expected actual)
      (passed actual)
      (report-error expected actual)))


;;;SECTION 1.1.1
(assert-equal 486 (+ 137 349))

(assert-equal 486 (+ 137 349))
(assert-equal 666 (- 1000 334))
(assert-equal 495 (* 5 99))
(assert-equal 2 (/ 10 5))
(assert-equal 12.7 (+ 2.7 10))
(assert-equal 75 (+ 21 35 12 7))
(assert-equal 1200 (* 25 4 12))
(assert-equal 19 (+ (* 3 5) (- 10 6)))
(assert-equal 57 (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)))
(assert-equal 57 (+ (* 3
                        (+ (* 2 4)
                           (+ 3 5)))
                     (+ (- 10 7)
                        6)))

(display "Passed: ")
(display pass)
(display " Errors: ")
(display errors)
(newline)
