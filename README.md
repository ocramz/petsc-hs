# petsc-hs

<p align="center">
  <a href="http://dev.stephendiehl.com/hask/">
    <img src="http://dev.stephendiehl.com/hask/img/title.png"/>
  </a>
</p>


[![GPL license](https://img.shields.io/badge/license-GPL-blue.svg)](https://github.com/ocramz/petsc-hs/master/LICENSE)

petsc-hs - Haskell bindings for the scientific computation library PETSc

(Portable Extensible Toolkit for Scientific Computation)

Copyright (c) 2015 - , Marco Zocca ( zocca at marco dot gmail at com )



Travis CI : [![Build Status](https://travis-ci.org/ocramz/petsc-hs.svg?branch=master)](https://travis-ci.org/ocramz/petsc-hs)

Discuss features, ideas, bugs : [![Gitter chat: petsc-hs](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/ocramz/petsc-hs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


## News

20/01/2016 : Experimenting with Docker containers : [petsc-hs-docker](https://github.com/ocramz/petsc-hs-docker) uses a pre-compiled image with PETSc and SLEPc  ([ocramz/petsc-docker](https://hub.docker.com/r/ocramz/petsc-docker/) on Docker Hub, that builds upon `debian:7.7`) and a derived image that pulls in the Haskell `stack` tool with which to build `petsc-hs` . 


6/12/2015 : We're integrating the [SLEPc](http://slepc.upv.es/) eigensolver suite, which is based on PETSc, and provides many options for spectral decomposition of sparse operators, e.g. (non)linear eigenproblem solvers, spectral transformation facilities for accelerated convergence, etc. 



## Introduction

The [PETSc](http://www.mcs.anl.gov/petsc/) architecture provides numerical data-structures and algorithms such as linear and nonlinear system solvers, extensive preconditioning facilities, time steppers (integrators) for dynamic problems, advanced meshing primitives for the solution of discretized PDEs and an advanced optimization toolkit. It is based on MPI, to enable distributed storage and computation.

Currently, `petsc-hs` wraps a core subset of PETSc (and the regular architecture of the C library makes expanding this subset quite straightforward) and 

* overlays static types (which in turn offer compile-time correctness guarantees on programs written with it),

* adds memory safety (allocation/deallocation pairs are lexical brackets inside which the users' programs are run, and the Haskell run-time garbage collector takes care of the details), and  

* enables functional compositionality (the logical soundness of larger programs is guaranteed by "clicking together", rationally, simpler programs).
  


## Vision

The grand aim of this library is to bring together functional programming and high-performance numerical computing, and in particular the guarantees provided by the former into the practice of the latter. Or, alternatively, to add scientific computation capabilities to a great programming language !

It is your humble author's opinion that many imperative languages do not completely address the needs of scientific programming: ease of design, of verification and of collaboration. 
Functional composition of sub-programs and rich, static types are the missing link between scientific programmer efficiency and program expressiveness.
 




## Installation


### 0 

Download and install the `stack` build tool following these [instructions](http://docs.haskellstack.org/en/stable/README.html). 

### 1

If you already have a working installation of PETSc, MPICH and SLEPc and the `PETSC_DIR`, `PETSC_ARCH`, `SLEPC_DIR` and `SLEPC_ARCH` environment variables are set, proceed to Step 2,

otherwise

* Install PETSc and MPICH. Download the archive from [here](http://www.mcs.anl.gov/petsc/download/index.html) and please refer to [this page](http://www.mcs.anl.gov/petsc/documentation/installation.html) for detailed configuration and installation instructions.

    _IMPORTANT_ : The environment variables denoting the PETSc architecture and root directories must be in the scope of the shell performing the next step. If they are not defined, the PETSc configuration step figures them out and  1. sets the PETSc root directory to where the PETSc archive has been decompressed, 2. compiles the dynamic libraries in a sub-directory whose name starts with `arch-`. In this case it's up to the user to export these as `PETSC_DIR` and `PETSC_ARCH`, respectively, before proceeding to installing PETSc and subsequently to Step 2.

    * We provide a default shell script to automate the PETSc download and installation, for a common single-node configuration: 
        - `./install-petsc.sh $PETSC_VERSION $PETSC_DIR $PETSC_ARCH` (`PETSC_VERSION=3.6.2`, for example ).



* Install SLEPc ; the complete instructions in this case are to be found in the [SLEPc manual](http://slepc.upv.es/documentation/slepc.pdf). 
    * For SLEPc too we provide a default download/install Bash script (single-node) that should be run as follows:
        - `./install-slepc.hs $SLEPC_VERSION $SLEPC_DIR`

    _IMPORTANT_ : Don't forget to export the SLEPc architecture (analogously to what we did before the PETSc installation) as `SLEPC_ARCH`.


### 2 

Clone the repository and enter its root directory: 

    git clone https://github.com/ocramz/petsc-hs.git && cd petsc-hs


### 3 

    ./stack-build.sh $STACK_ARGS $PETSC_DIR $PETSC_ARCH $SLEPC_DIR $SLEPC_ARCH

This compiles the whole project, library and examples, part of which into object code. 
It is just a synonym for `stack build` that uses the PETSc/SLEPc root directory and architecture variables, as specified above, to look for the include files and dynamic libraries.

The `STACK_ARGS` variable can be used to build against specific Stackage repositories, or can be left as the empty string.


### 4 

Now you can try out an example by running 

    stack exec petsc-example 

The binaries using `petsc-hs` will be linked both with the PETSc dynamic libraries and with those produced by Step 3.



## Dependencies 

The library is being developed with/on :

* PETSc 3.6.0, 3.6.2 (October 2015) (and MPICH 3.1.1)

* C and Fortran compilers : `gcc` 5.0.0 , `gfortran` 5.0.0

* OS : OSX 10.9.5, Ubuntu 12.04 LTS Server Edition 64 bit on Travis CI

* Haskell compiler : `ghc` 7.8.3, 7.10.2 

* Cabal 1.22

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
