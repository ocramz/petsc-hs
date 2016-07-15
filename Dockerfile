FROM ocramz/petsc-docker

# MAINTAINER Marco Zocca < github.com/ocramz >

ENV SWDIR=/opt

COPY setup.sh ${SWDIR}/
COPY c2hs-build.sh ${SWDIR}/
COPY stack-build.sh ${SWDIR}/
COPY stack-exec-example.sh ${SWDIR}/

WORKDIR ${SWDIR}

RUN ./setup.sh ${SWDIR}
   