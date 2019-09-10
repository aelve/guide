# Aelve Guide

[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aelve/guide/blob/master/LICENSE)

The production version is running at [guide.aelve.com](https://guide.aelve.com). The new frontend is being developed at [staging.guide.aelve.com](https://staging.guide.aelve.com).

Installation instructions and the explanation of config variables (in `config.json`) are here: [INSTALL.md](INSTALL.md).

## Benchmarking

Start Postgres and create the `guide` database.

If you've been building with `stack build --fast`, do a cleanup:

```
stack clean
```

Then build and run benchmarks:

```
stack bench
```
