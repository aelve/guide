# [WIP] Guide's front-end

## Requirements

* [`Node`](https://nodejs.org/en/)
* [`yarn`](https://yarnpkg.com/lang/en/) (or [npm](https://www.npmjs.com/))
* [`Pulp`](https://github.com/purescript-contrib/pulp)


## Installation

1. Get all sources:
```sh
git clone git@github.com:aelve/guide.git
cd guide
git checkout front-ps-next
```

2. Get data of `guide-database`
```sh
./official.sh
```

3. Build back-end API
```
./b
```

4. Install front-end dependencies
```sh
yarn
```

5. Build Express server
```sh
yarn build:server
```

6. Build page `category-overview`
```sh
yarn build:category-overview
```

7. Build page `category-detail`
```sh
yarn build:category-detail
```

## Run

1. Run back-end
```sh
stack exec guide
```

2. Run front-end
```sh
yarn start
```

Open [`http://localhost:3333`](http://localhost:3333).
