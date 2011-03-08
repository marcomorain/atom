# Atom
### As small as I can make a *complete* R5RS Scheme implementation.

### TODO:
* Macros
* Debug data on each Cell (file and line and column were it was created).
* C API for user functions
* Userdata / void* / C / C++ data types? (with gc callback)
* add const flag to data if it is compile time created data.
* Add proper tail recursion for if

## Report
### Chapter 4: Expressions
#### Done 
* quote
* lambda
* if
* set!
* cond
* and
* or
* begin

#### Not Done
* case
* let
* let*
* letrec
* do
* let (named)
* delay
* quasiquote <qq template>
* let-syntax
* letrec-syntax
* syntax-rules

### Chapter 5: Program structure
#### Done
* define

#### Not Done
* define-syntax

### Chapter 6: Standard Procedures
#### 6.1. Equivalence predicates
##### Done
* eqv?
* eq?

##### Not Done
* equal?

#### 6.2. Numbers
##### Done
* number?
* complex?
* real?
* rational?
* integer?
* =
* <
* >
* <=
* >=
* max
* min
* +
* *
* -
* /
* modulo

##### Not Done
* exact? 
* inexact?
* zero?
* positive?
* negative?
* odd?
* even?
* abs
* quotient
* remainder
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

##### Not Done
* number->string
* string->number

## Booleans
* not
* boolean?

## Pairs and lists
##### Done
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

##### Not Done
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
##### Done
* symbol?
* symbol->string
* string->symbol

## 6.3.4. Characters
##### Done
* char?
* char->integer
* integer->char

##### Not Done
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
* char-upcase
* char-downcase

## 6.3.5 Strings

##### Done
* string?
* make-string
* string-length
* string-ref
* string-set!

##### Not Done
* string
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

##### Done
* vector?
* make-vector
* vector-length
* vector-ref
* vector-set!
* vector->fill!

##### Not Done
* vector
* vector->list
* list->vector

### 6.4 Control Features
##### Done
* procedure?
* apply?

##### Not Done
* map
* for-each
* force
* call-with-current-continuation
* values
* call-with-values
* dynamic-wind

### 6.5 Eval
##### Not Done
##### Not Done
* eval
* scheme-report-environment
* null-environment
* interaction-environment

### 6.6.1 Ports
##### Not Done
##### Not Done
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
##### Done
##### Not Done
* read
* read-char
* peek-char
* eof-object?
* char-ready?

### 6.6.3 Output

##### Done
* write
* display
* newline

##### Not Done
* write-char

### 6.6.4 Output
##### Done
* load

##### Not Done
* transcript-on
* transcript-off