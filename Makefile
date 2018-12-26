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

# Test that the official database can be loaded
.PHONY: back/test-db
back/test-db: back
	if [ -d back/state ]; then mv back/state back/state-old; fi
	git clone --depth 1 https://github.com/aelve/guide-database.git
	mv guide-database back/state
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
# Docker file)
.PHONY: back/travis-docker
back/travis-docker:
	rm -rf docker/back/files && mkdir docker/back/files
	git clone --depth 1 https://github.com/aelve/guide-database.git \
		docker/back/files/state
	rm -rf docker/back/files/state/.git
	cp .stack-work/install/*/*/*/bin/guide docker/back/files/
	cp -R back/{config.json,static,templates} docker/back/files/
	docker build docker/back/files -t quay.io/aelve/guide:$(tag)
	rm -rf docker/back/files
