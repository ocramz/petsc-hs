Information for petsc-hs Developers

## Project structure

The low-level bindings use `inline-c` (https://github.com/fpco/inline-c) to wrap the individual PETSc C functions into Template Haskell statements (see ../Internal/InlineC.hs) and to map the C types onto Haskell types (see ../Internal/Internal.hs). 
 
Exception control can be found in ..Internal/Exception.hs ; the integer error codes returned by the "raw" calls (those in ../Internal/InlineC.hs) are checked and an appropriate exception is thrown.

The mid-level bindings (../Internal/PutGet.hs and related) wrap the low-level calls with the exception logic and group appropriately allocation-setup-cleanup sequences into higher level "with-" functions, using e.g. `bracket` from Control.Exception . An application developer should only work with these or higher-level bindings (PETSc is an extensive library and can be used in *many* different ways, so it's better not to limit users by design too much). The types for the mid-level bindings are declared in ..Internal/Types.hs .


## Build Sequence

UPDATE : stack-based installation works, use that rather than the makefile. See README.md

(1) ghc compiles the inline-c parts into C sources

(2) a C compiler builds the files produced by (1) into dynamic libraries, while also importing the PETSc headers

(3) ghci compiles some example file that uses the mid-level bindings and dynamically links the object files built in (2)





