# Atom
### As small as I can make a *complete* R5RS Scheme implementation.

### TODO:
* Syntax:
    * case
* Macros
* Garbage collector
* Debug data on each Cell (file and line and column were it was created).
* call-with-current-continuation
* C API for user functions
* Userdata / void* / C / C++ data types? (with gc callback)
* delay and force
* vector type
* add const flag to data if it is compile time created data.
* Libraries:
    * string
    * input
    * output
    * math
    * symbol
* Add proper tail recursion for if

## Report
### Chapter 4: Expressions
* <del>quote</del>
* lambda
* <del>if</del>
* <del>set!</dev>
* <del>cond</del>
* case
* <del>and</del>
* <del>or</del>
* let
* let*
* letrec
* <del>begin</del>
* do
* let (named)
* delay
* quasiquote <qq template>) 
* let-syntax
* letrec-syntax
* syntax-rules

### Chapter 5: Program structure
* <del>define</del>
* define-syntax

### Chapter 6: Standard Procedures
* eqv?
* eq?
* equal?
* number?
* complex?
* real?
* rational?
* integer?
* exact? 
* inexact?
* =
* <
* >
* <=
* >=
* zero?
* positive?
* negative?
* odd?
* even?
* <del>max</del>
* <del>min</del>
* <del>+</del>
* <del>*</del>
* <del>-</del>
* <del>/</del>
* abs
* quotient
* remainder
* modulo
* gcd
* lcm
* numerator
* denominator
* floor
* ceiling
* round
* rationalize
* exp  
* log  
* sin  
* cos  
* tan 
* asin 
* acos
* atan
* sqrt
* expt
* make-rectangular
* make-polar
* real-part
* imag-part
* magnitude
* angle
* exact->inexact
* inexact->exact
## 6.2.6  Numerical input and output
* number->string
* string->number

## Booleans
* not
* boolean

## Pairs and lists
* pair?
* cons
* car
* cdr
* set-car!
* set-cdr!
* caar - cddddr
* null?
* list?
* list
* length
* append
* reverse
* list-tail
* list-ref
* memq
* memv
* member
* assq
* assv
* assoc

## 6.3.3 Symbols
* symbol?
* symbol->string
* string->symbol
* char?
* char=?
* char<?
* char>?
* char<=?
* char>=?
* char-ci=?
* char-ci<?
* char-ci>?
* char-ci<=?
* char-ci>=?
* char-aphabetic?
* char-numeric?
* char-whitespace?
* char-upper-case?
* char-lower-case?
* char->integer
* integer->char
* char-upcase
* char-downcase

## 6.3.5 Strings

* <del>string?</del>
* make-string
* string
* string-length
* string-ref
* string-set!
* string=?
* string-ci=?
* string<?
* string>?
* string<=?
* string>=?
* string-ci<?
* string-ci>?
* string-ci<=?
* string-ci>=?
* substring
* string-append
* string->list
* list->string
* string-copy
* string-fill!

## 6.3.6 Vectors
* vector?
* make-vector
* vector
* vector-length
* vector-ref
* vector-set!
* vector->list
* list->vector
* vector->fill!

### 6.4 Control Features
* procedure?
* apply?
* map
* for-each
* force
* call-with-current-continuation
* values
* call-with-values
* dynamic-wind

### 6.5 Eval
* eval
* scheme-report-environment
* null-environment
* interaction-environment

### 6.6.1 Ports

* call-with-input-file
* call-with-output-file
* input-port?
* output-port?
* current-input-port
* current-output-port
* with-input-from-file
* with-output-to-file
* open-input-file
* open-output-file
* close-input-file
* close-output-file

### 6.6.3 Input
* read
* read-char
* peek-char
* eof-object?
* char-ready?

### 6.6.3 Output
* write
* display
* newline
* write-char

### 6.6.4 Output
* load
* transcript-on
* transcript-off