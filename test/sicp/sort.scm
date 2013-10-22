(define (same-length lst)   
  (sort (map string-length lst)))
(define a (list "one" "two" "three" "four" "five" "six" "seven" "eight"))

(define (selection-sort lst)
  (if (null? lst)
      '()
      (selection-sort-helper (smallest lst) lst)))
  
  ; The helper procedure handles the deletion of the smallest
  ; element and the repetition
  (define (selection-sort-helper smallest-value lst)
     (cons smallest-value 
          (selection-sort (list-remove lst smallest-value))))

(define (smallest lst)
  (if (null? (cdr lst))
      (car lst)
      (smaller (car lst) (smallest (cdr lst)))))

(define (smaller val1 val2)
  (if (< val1 val2) val1 val2))

(define (list-remove lst val)
  (if (eq? val (car lst))
      (cdr lst)
      (cons (car lst) (list-remove (cdr lst) val))))

(define sort selection-sort)
(define b (map string-length a))