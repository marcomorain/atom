# Atom
### As small as I can make a *complete* R5RS Scheme implementation.

### TODO:
* Write repl loop
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