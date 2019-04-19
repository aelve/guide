FROM ubuntu:bionic

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

ARG PORT_ARG=5000
ENV PORT=$PORT_ARG

ARG API_URL_ARG=https://staging.guide.aelve.com:4400/
ENV API_URL=$API_URL_ARG

RUN apt update

RUN curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | bash

RUN vm install v11.14.0

WORKDIR /opt/front

COPY files ./

CMD node ./dist/server.js
