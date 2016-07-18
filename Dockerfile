FROM ocramz/petsc-docker

# MAINTAINER Marco Zocca < github.com/ocramz >

ENV PETSC_INCLUDE1=${PETSC_DIR}/include/ \
    PETSC_INCLUDE2=${PETSC_DIR}/${PETSC_ARCH}/include/ \
    PETSC_LIB=${PETSC_DIR}/${PETSC_ARCH}/lib/ \
    SLEPC_INCLUDE1=${SLEPC_DIR}/include/ \
    SLEPC_INCLUDE2=${SLEPC_DIR}/${SLEPC_ARCH}/include/ \
    SLEPC_LIB=${SLEPC_DIR}/${SLEPC_ARCH}/lib/

ENV PETSCHS_DIR=${SWDIR}/petsc-hs \
    LD_LIBRARY_PATH=${PETSC_LIB}:${SLEPC_LIB}:${LD_LIBRARY_PATH}

COPY petsc-hs-setup.sh update-petsc-hs.sh ${SWDIR}/

WORKDIR ${SWDIR}

RUN ./petsc-hs-setup.sh ${SWDIR}