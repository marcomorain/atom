`(list ,(+ 1 2) 4)

`(list ,(+ 1 2) 4)
; (list 3 4)

(let ((name 'a))
    `(list ,name ',name))
; (list a (quote a))

`(a ,(+ 1 2) ,@'(4 5 6) b)
; (a3456b)

`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))

; ((foo 7) . cons)

;; `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8) =⇒ #(1052438)

(current-output-port)
; CRASH
;(input-port? 1)
;(if (input-port? 1) (display "yes"))
;(if (output-port? (current-output-port)) (display "win"))
;(if (output-port? (current-output-port)) (display "win"))
;(if (output-port? (current-output-port)) (display "win"))
;(if (output-port? (current-input-port)) (display "True"))