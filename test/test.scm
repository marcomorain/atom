
(define error
  (lambda (msg)
    (display msg)
    (newline)))

(define assert
	(lambda (c)
		(if (not c)
			(error "assertion failed"))))

(assert (= (+ 1 2) 4))

;(- 3 4)
;(- (a . b) (c . d))

(if #t 1)
(if #f 1 2)
(if #f 1)

(assert #t)

(assert 1)

(assert '())
(assert (cons 1 2))
'()

; Set awy to one
(define awy 1)

; Then see what awy is
awy

(cons 1 2)

(define marc (cons 3 4))

marc

(car marc)
(cdr marc)

(define finnegan #f)
finnegan

(define true #t)

(define bools (cons true finnegan))
(car bools)
(cdr bools)

(boolean? #t)
(boolean? #f)
(boolean? 1)
(boolean? marc)

(number? 1)
(number? awy)
(number? #t)
(number? #f)
; bug here  (number? #\s)
(number? #\space)

(pair? marc)
(pair? #t)
(pair? #f)
(pair? #\newline)
(pair? bools)
(pair? awy)


bools
(set-car! bools #f)
(set-cdr! bools #t)
bools

(list? (quote(a b c))) ; => #t
(list? (quote()))	     ; => #t
(list? (quote(a . b))) ; => #f
(list? '(a b c))

(+ 1 1 1 1)
(* 1)
(*)

(> 1 2)
(> 3 2 3)
(> 2 1)
(> 2 1 0)
(> 2 1 3)


(cond ((> 3 3) 'greater)
			((< 3 3) 'less)
			(else 'equal))

(cond ((> 3 2) 'greater)
			((< 3 2) 'less))
									
;(cond ((assv 'b '((a 1) (b 2))) => cadr)
;		(else #f))

(= 1 2)
(= 1 1 1)
(= 1 1 1 4)
(= 1 1 1)
(assert #t)

(load "/Users/marcomorain/dev/scheme/test/print.scm")

(apply + '(1 2 3 ))

(define printer
  (lambda (a b c)
		(define inner
			(lambda (d)
				(+ a d)))
		(display (inner 2))
                (newline)
	     	'done))
(printer 1 2 3)

(define (add a b c)
		(+ a b c))
(add 3 3 3)

(symbol->string add)
(make-string 5)
(make-string 5 #\a)


