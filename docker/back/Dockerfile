FROM ubuntu:xenial

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

RUN apt update && apt install -y libgmp-dev libpq-dev

WORKDIR /opt/guide

COPY files ./

CMD ./guide
