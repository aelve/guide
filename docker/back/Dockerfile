FROM ubuntu:xenial

RUN apt update && apt install -y libgmp-dev

WORKDIR /opt

COPY guide ./
COPY state ./state

CMD ./guide
