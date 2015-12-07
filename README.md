# petsc-hs

petsc-hs - Haskell bindings for the scientific computation library PETSc

(Portable Extensible Toolkit for Scientific Computation)

Copyright (c) 2015 - , Marco Zocca ( zocca at marco dot gmail at com )



Travis CI : [![Build Status](https://travis-ci.org/ocramz/petsc-hs.svg?branch=master)](https://travis-ci.org/ocramz/petsc-hs)

Discuss features, ideas, bugs : [![Gitter chat: petsc-hs](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/ocramz/petsc-hs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


## News

6/12/2015 : We're integrating the [SLEPc](https://slepc.upv.es/) eigensolver suite, which is based on PETSc, and provides many options for spectral decomposition of sparse operators, e.g. (non)linear and polynomial eigenproblem solvers, spectral transformation facilities for accelerated convergence, etc. 



## Introduction

The [PETSc](http://www.mcs.anl.gov/petsc/) architecture provides numerical data-structures and algorithms such as linear and nonlinear system solvers, extensive preconditioning facilities, time steppers (integrators) for dynamic problems, advanced meshing primitives for the solution of discretized PDEs and an advanced optimization toolkit. It is based on MPI, to support distributed storage and computations.

Currently, `petsc-hs` wraps a core subset of PETSc (and the regular architecture of the C library makes expanding this subset quite straightforward) and 

* overlays static types (which in turn offer compile-time correctness guarantees on programs written with it),

* adds memory safety (allocation/deallocation pairs are lexical brackets inside which the users' programs are run, and the Haskell run-time garbage collector takes care of the details), and  

* enables functional compositionality (the logical soundness of larger programs is guaranteed by "clicking together", rationally, simpler programs).
  


## Vision

The grand aim of this library is to bring together functional programming and high-performance numerical computing, and in particular the guarantees provided by the former into the practice of the latter. Or, alternatively, to add scientific computation capabilities to a great programming language !

It is your humble author's opinion that many imperative languages do not completely address the needs of scientific programming: ease of design, of verification and of collaboration. 
Functional composition of sub-programs and rich, static types are the missing link between scientific programmer efficiency and program expressiveness.
 




## Installation

If you already have a working PETSc installation, the PETSC_DIR and PETSC_ARCH environment variables are set and `stack` is installed and in PATH:

* `git clone https://github.com/ocramz/petsc-hs.git`
* `cd petsc-hs` 
* `make stack_build`
* `stack exec petsc-example`

otherwise

* Obtain the `stack` build tool as documented [here](http://docs.haskellstack.org/en/stable/README.html).

* A working installations of PETSc and MPICH (both configured as dynamic libraries) is required. Download the archive from [here](http://www.mcs.anl.gov/petsc/download/index.html) and please refer to [this page](http://www.mcs.anl.gov/petsc/documentation/installation.html) for detailed configuration and installation instructions. The default `configure` command suggested on the PETSc Installation page Just Works (TM), and if it doesn't, there will be plenty of well-formatted output to understand what went wrong.

_IMPORTANT_ : The string variables denoting the PETSc architecture and root directories must be in the scope of the shell performing the installation of the Haskell bindings: the user's Bash .profile should contain the following two lines: `export PETSC_DIR=<PETSc root dir>` and `export PETSC_ARCH=<build target dir>`. 

We also provide a default shell script to automate the PETSc download and installation, for convenience: `install-petsc.sh`.


* Once the dependencies are setup, run `make stack_build`. This is just a synonym for `stack build` that uses the PETSc root directory and architecture variables, as specified above.

* Now you can try out an example by running `stack exec petsc-example`.



## Dependencies 

The library is being developed with/on :

* PETSc 3.6.0, 3.6.2 (October 2015) (and MPICH 3.1.1)

* OS : OSX 10.9.5, Ubuntu 12.04 LTS Server Edition 64 bit on Travis CI

* Haskell compiler : GHC 7.8.3, 7.10.2 

* Cabal 1.22

* `inline-c` > 0.5.5

* `stack` 0.1.4.0









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
