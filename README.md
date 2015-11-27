# petsc-hs
petsc-hs - Haskell bindings for the scientific computation library PETSc

(Portable Extensible Toolkit for Scientific Computation)

Copyright (c) 2015 - , Marco Zocca ( zocca at marco dot gmail at com )



Travis CI : [![Build Status](https://travis-ci.org/ocramz/petsc-hs.svg?branch=master)](https://travis-ci.org/ocramz/petsc-hs)


## Introduction

The [PETSc](http://www.mcs.anl.gov/petsc/) architecture provides numerical data-structures and algorithms such as linear and nonlinear system solvers, extensive preconditioning facilities, time steppers (integrators) for dynamic problems, advanced meshing primitives for the solution of discretized PDEs and an advanced optimization toolkit. It is based on MPI, to support distributed storage and computations.

This Haskell library wraps many of the C functions while adding types, memory safety and compositionality. 


## Vision

The grand aim of this library is to bring together functional programming and high-performance numerical computing, and in particular the guarantees provided by the former into the practice of the latter. Or, alternatively, to add scientific computation capabilities to a great programming language !

It is your humble author's opinion that many imperative languages do not completely address the needs of scientific programming: ease of design, of verification and of collaboration. 
Functional composition of sub-programs and rich, static types are the missing link between scientific programmer efficiency and program expressiveness.
 




## Installation

* First of all, a working installations of PETSc and MPICH (both configured as dynamic libraries) is required. Download the archive from [here](http://www.mcs.anl.gov/petsc/download/index.html) and please refer to [this page](http://www.mcs.anl.gov/petsc/documentation/installation.html) for detailed configuration and installation instructions.

* The Haskell side of the bindings is based on `inline-c`, which can be obtained from Hackage via `cabal install inline-c`.

* _IMPORTANT_ : The PETSc architecture and import directories are _hardcoded_ (for now) within the makefile and the .cabal file ("arch-darwin-c-debug"), but they depend on the actual configuration parameters you supplied when installing the library ; modify them to suit your case.

* Once the dependencies are setup, run `stack build` (assuming you have already setup the `stack` tool as documented [here](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md) ). There is also currently a makefile option (`make`) which however we plan to phase out in the future. 

* After `stack build` you can try out an example by running `stack exec petsc-example`.



## Dependencies 

The library is being developed with/on :

* PETSc 3.6.0 and 3.6.2 (October 2015)

* OS : OSX 10.9.5 (but most other Unices should work, mutatis mutandis)

* Haskell compiler : GHC 7.8.3 and 7.10.2

* `inline-c` > 0.5.5










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
