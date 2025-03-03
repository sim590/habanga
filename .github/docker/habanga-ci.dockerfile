
FROM ghcr.io/sim590/habanga-deps:latest
LABEL maintainer="Simon Désaulniers <sim.desaulniers@gmail.com>"
LABEL org.opencontainers.image.source="https://github.com/sim590/habanga"

RUN apt update
RUN apt install sudo curl

