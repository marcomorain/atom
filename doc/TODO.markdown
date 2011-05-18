### TODO:
* Macros
* Debug data on each Cell (file and line and column were it was created).
* C API for user functions
* Userdata / void* / C / C++ data types? (with gc callback)
* add const flag to data if it is compile time created data.
* Add proper tail recursion for if

## Report
### Chapter 4: Expressions
* case
* letrec
* do
* let (named)
* delay
* quasiquote <qq template>
* let-syntax
* letrec-syntax
* syntax-rules

### Chapter 5: Program structure
* define-syntax

### Chapter 6: Standard Procedures
#### 6.1. Equivalence predicates
#### 6.2. Numbers
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

* number->string
* string->number

## Booleans
## Pairs and lists
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
## 6.3.4. Characters
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

### 6.4 Control Features
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

### 6.6.3 Input
* read
* read-char
* peek-char
* eof-object?
* char-ready?