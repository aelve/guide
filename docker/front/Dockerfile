FROM ubuntu:bionic

ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8

# NODE_ENV used to run front server on production
ENV NODE_ENV=production

# Overrides for variables in front/config.js
ENV API_URL=https://staging.guide.aelve.com:4400
ENV PORT=5000

# Install NodeJS via nvm

# replace shell with bash so we can source files
RUN rm /bin/sh && ln -s /bin/bash /bin/sh

# update the repository sources list
# and install dependencies
RUN apt-get update \
    && apt-get install -y curl \
    && apt-get -y autoclean

# nvm environment variables
ENV NVM_DIR /usr/local/nvm
ENV NODE_VERSION 11.14.0

# install nvm
# https://github.com/creationix/nvm#install-script
RUN curl --silent -o- https://raw.githubusercontent.com/creationix/nvm/v0.31.2/install.sh | bash

# install node and npm
RUN source $NVM_DIR/nvm.sh \
    && nvm install $NODE_VERSION \
    && nvm alias default $NODE_VERSION \
    && nvm use default

# add node and npm to path so the commands are available
ENV NODE_PATH $NVM_DIR/v$NODE_VERSION/lib/node_modules
ENV PATH $NVM_DIR/versions/node/v$NODE_VERSION/bin:$PATH

# confirm installation
RUN node -v
RUN npm -v

WORKDIR /opt/front

# Copy front server to docker container
COPY files ./

CMD node ./dist/server
