FROM ubuntu:xenial

RUN apt update && apt install -y libgmp-dev

WORKDIR /opt/guide

COPY files ./

CMD ./guide
