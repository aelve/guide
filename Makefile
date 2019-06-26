SHELL := /bin/bash

# Build the project
.PHONY: back
back:
	stack build --fast

# Run tests
.PHONY: back/test
back/test:
	stack test --fast

# Download the official database
.PHONY: back/db
back/db:
	rm -rf back/state
	git clone --depth 1 https://github.com/aelve/guide-database.git
	mv guide-database back/state
	(cd back/state && gzip -d *.gz)

# Test that the official database can be loaded
.PHONY: back/test-db
back/test-db: back
	if [ -d back/state ]; then mv back/state back/state-old; fi
	git clone --depth 1 https://github.com/aelve/guide-database.git
	mv guide-database back/state
	(cd back/state && gzip -d *.gz)
	(cd back/state && git branch -v && git status && ls)
	stack exec --cwd back -- guide --dry-run
	rm -rf back/state
	if [ -d back/state-old ]; then mv back/state-old back/state; fi

# Run the backend
.PHONY: back/run
back/run:
	stack exec --cwd back -- guide

# Create a Docker image for the backend; will only work on Travis because
# the binary has to have been compiled on Ubuntu Xenial (the OS used in the
# Docker file).
#
# Assumes Guide has been built with Stack.
.PHONY: back/travis-docker
back/travis-docker:
	rm -rf docker/back/files && mkdir docker/back/files
	git clone --depth 1 https://github.com/aelve/guide-database.git \
		docker/back/files/state
	(cd docker/back/files/state && gzip -d *.gz)
	rm -rf docker/back/files/state/.git
	cp "$(stack path --local-install-root)/bin/guide" docker/back/files/
	cp -R back/{static,templates} docker/back/files/
	docker build docker/back -t quay.io/aelve/guide:$(tag)
	rm -rf docker/back/files

# Create a Docker image for the front;
.PHONY: front/travis-docker
front/travis-docker:
	rm -rf docker/front/files && mkdir docker/front/files
	cp -R front/dist docker/front/files/
	cd ./docker/front/files/dist/ && export NODE_ENV=production && npm install
	docker build docker/front -t quay.io/aelve/guide:$(tag)
	rm -rf docker/front/files
