### TODO:

#### Alpha 1
* Implement define with varying number of paramters
* Test suite should pass
* Packages

#### Alpha 2
* Code coverage
* Simple Macros
* Full hygenic macros
* Full number tower
* Full number parsing
* Fix all warnings
* Call CC up only
* Full call CC
* Debug data on each Cell (file and line and column were it was created).
* C API for user functions
* Userdata / void* / C / C++ data types? (with gc callback)
* add const flag to data if it is compile time created data.
* Add proper tail recursion for if
* Implement C library with the API

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
* quotient
* remainder
* gcd
* lcm
* numerator
* denominator
* rationalize
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
* memq
* memv
* member
* assq
* assv
* assoc


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

### 6.6.1 Ports
* call-with-input-file
* call-with-output-file

### 6.6.3 Input
* read
* char-ready?
