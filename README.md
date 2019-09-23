# Aelve Guide

[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aelve/guide/blob/master/LICENSE)

The production version is running at [guide.aelve.com](https://guide.aelve.com). The new frontend is being developed at [staging.guide.aelve.com](https://staging.guide.aelve.com).

Installation instructions and the explanation of config variables (in `config.json`) are here: [INSTALL.md](INSTALL.md).

## Benchmarking

Start Postgres and create an empty database called `guide-bench`. An example
with Docker:

```
docker run --name guide-db -e POSTGRES_PASSWORD=3 -e POSTGRES_DB=guide-bench -p 5432:5432 -d postgres
```

Build and run benchmarks:

```
stack bench
```

If you have been building with `--fast` previously, or using `make`, Stack
will detect that Guide has to be recompiled with `-O` and do it
automatically.
