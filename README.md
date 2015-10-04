# petsc-hs
petsc-hs - Haskell bindings for the scientific computation library PETSc

(Portable Extensible Toolkit for Scientific Computation)
Copyright (c) 2015 - , Marco Zocca ( zocca at marco dot gmail at com )



## Introduction

The [PETSc](http://www.mcs.anl.gov/petsc/) architecture provides numerical data-structures and algorithms such as linear and nonlinear equation solvers and extensive preconditioning facilities, time steppers (integrators) for dynamic problems, advanced meshing primitives for the solution of discretized PDEs and an advanced optimization toolkit. It is based on MPI, to support distributed storage and computations.

This Haskell library wraps many of the C functions while adding type safety and compositionality. As a result, programs are much shorter, therefore easier to interpret, debug, study and improve.


## Vision

The grand aim of this library is to bring together functional programming and high-performance numerical computing, and in particular the guarantees provided by the former into the practice of the latter.

It is your humble author's opinion that many imperative languages do not completely address the needs of scientific programming: ease of design, of verification and of collaboration. 
Functional composition of sub-programs and rich, static types are the missing link between scientific programmer efficiency and program expressiveness.
 




## Installation

* First of all, a working installations of PETSc (configured as a dynamic library) is required. Please refer to the respective pages for detailed instructions.

* The Haskell side is based on `inline-c`, which can be obtained from Hackage via `cabal install inline-c`.

* Once everything is setup simply run `make` from within the petsc-hs root directory, and at the end of the process you should find yourself within a GHCi interactive session.




## Notes

* The library is being developed on a Unix, with the Haskell compiler GHC 7.8.4, on top of PETSc 3.6.0 using MPICH (installed via the PETSc installation process).

* The PETSc architecture directory within the makefile is hardcoded ("arch-darwin-c-debug"), but it depend on the actual configuration parameters you supplied when installing the library ; modify it to suit your case.





## License

petsc-hs is free software: you can redistribute it and/or modify it under the
terms of version 3 of the GNU Lesser General Public License as published by
the Free Software Foundation.
petsc-hs is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for
more details.
You should have received a copy of the GNU Lesser General Public License
along with slepc-hs. If not, see <http://www.gnu.org/licenses/>.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
