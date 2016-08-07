<p align="center">
  <a href="https://github.com/ocramz/petsc-hs">
    <img src="https://github.com/ocramz/petsc-hs/blob/master/doc/img/logo.jpg" />
  </a>
</p>




#### Haskell bindings for PETSc (Portable Extensible Toolkit for Scientific Computation)

Copyright (c) 2015 - , Marco Zocca ( zocca at marco dot gmail at com ) - [![GPL license](https://img.shields.io/badge/license-GPL-blue.svg)](https://github.com/ocramz/petsc-hs/blob/master/LICENSE)

Travis CI : [![Build Status](https://travis-ci.org/ocramz/petsc-hs.svg?branch=master)](https://travis-ci.org/ocramz/petsc-hs)



Discuss features, ideas, bugs : [![Gitter chat: petsc-hs](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/ocramz/petsc-hs?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


## News

July 19, 2016 : petsc-hs now ships also as a Docker image, so the [petsc-hs-docker](https://github.com/ocramz/petsc-hs-docker) project becomes redundant. Please use only the image provided [here](https://hub.docker.com/r/ocramz/petsc-hs) from now on.

July 5, 2016 : Merged branch `petsc-3.7` into `master`: Development will from now on target PETSc 3.7, due to some breaking API changes with respect to 3.6. Also, we are GHC 8.0.1-compatible, and using Stackage (`nightly-2016-06-20`)for reproducible dependencies.

June 28-30, 2016 : Presented petsc-hs at the [PETSc User Meeting](https://www.mcs.anl.gov/petsc/meetings/2016/program.html) in Vienna, [poster in PDF](https://github.com/ocramz/petsc-hs/blob/master/doc/posters/petsc-hs_PETSc_2016.pdf).

June 8, 2016 : Building petsc-hs against the latest PETSc (3.7.2, rel. June 5, 2016) and SLEPc (3.7.1, rel. May 27, 2016). See branch `petsc-3.7`.

December 6, 2015 : We're integrating the [SLEPc](http://slepc.upv.es/) eigensolver suite, which is based on PETSc, and provides many options for spectral decomposition of sparse operators, e.g. (non)linear eigenproblem solvers, spectral transformation facilities for accelerated convergence, etc. 



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

With `stack`, install the `c2hs` tool (used for generating low-level parts of the C bindings)

    $ stack install c2hs

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

    ./stack-build.sh "$STACK_ARGS" "$PETSC_DIR" "$PETSC_ARCH" "$SLEPC_DIR" "$SLEPC_ARCH"

This compiles the whole project, library and examples. 
It is just a synonym for `stack setup && stack build` that uses the PETSc/SLEPc root directory and architecture variables, as specified above, to generate the include and object code directories.

The `STACK_ARGS` variable can be used to build against specific Stackage repositories, or can be left as the empty string.

Example : `./stack-build.sh "" "$HOME/petsc" "arch-linux2-c-debug" "$HOME/slepc" "arch-linux2-c-debug"`


### 4 

Now you can try out an example by running 

    stack exec petsc-example 

The binaries using `petsc-hs` will be linked both with the PETSc dynamic libraries and with those produced by Step 3.




## Usage (Docker image)

0. (make sure Docker is installed, preferably at the latest version, and if running on OSX or Windows a `docker-machine` VM should be up and running)

1. Download the image with `docker pull ocramz/petsc-hs`

2. Run the image with `docker run --rm -it ocramz/petsc-hs /bin/bash` and, at its prompt, build the latest version of `petsc-hs` with  `./update-petsc-hs.sh`. This will run the examples and leave the image open for experimentation.






## Dependencies 

The library is being developed with/on :

* PETSc ~~3.6.0, 3.6.2 (October 2015),~~ 3.7.2 (June 2016) (and MPICH 3.1.1)

* C and Fortran compilers : `gcc` 5.0.0 , `gfortran` 5.0.0

* OS : OSX 10.9.5, Ubuntu Linux 12.04, 14.04

* Haskell compiler : `ghc` 7.10.3 (Linux only), 8.0.1 (Linux and OSX)

* Cabal 1.22

* `stack` 1.0.4, 1.1.0


## Compatibility Notes

June 10, 2016 : There seems to be a linker bug in GHC version 7.10.* for OSX preventing the use of cross-referencing dynamic libraries, which is why `petsc-hs` will make the transition to GHC 8 as soon as all its dependencies will be met (expected end of 2016).








## License

petsc-hs is free software: you can redistribute it and/or modify it under the
terms of version 3 of the GNU General Public License as published by
the Free Software Foundation.
petsc-hs is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License for
more details.
You should have received a copy of the GNU General Public License
along with petsc-hs. If not, see <http://www.gnu.org/licenses/>.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
