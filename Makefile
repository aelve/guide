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
