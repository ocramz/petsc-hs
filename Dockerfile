FROM ocramz/petsc-docker

# MAINTAINER Marco Zocca < github.com/ocramz >

ENV SWDIR=/opt \
    LD_LIBRARY_PATH=${PETSC_LIB}:${SLEPC_LIB}:${LD_LIBRARY_PATH}

ENV PETSCHS_DIR=${SWDIR}/petsc-hs

COPY setup.sh c2hs-build.sh stack-build.sh update-petsc-hs.sh stack-exec-example.sh ${SWDIR}/

WORKDIR ${SWDIR}

RUN ./setup.sh ${SWDIR}
   