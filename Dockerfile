FROM ocramz/petsc-docker

MAINTAINER Marco Zocca < github.com/ocramz >

ENV SWDIR=/opt

COPY setup.sh ${SWDIR}/
WORKDIR ${SWDIR}

RUN ./setup.sh ${SWDIR}
   