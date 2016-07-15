FROM ocramz/petsc-docker

# MAINTAINER Marco Zocca < github.com/ocramz >

ENV SWDIR=/opt

COPY setup.sh c2hs-build.sh stack-build.sh stack-exec-example.sh ${SWDIR}/

WORKDIR ${SWDIR}

RUN ./setup.sh ${SWDIR}
   