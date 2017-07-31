# [WIP] Guide's front-end

## Requirements

* [`Node`](https://nodejs.org/en/)
* [`yarn`](https://yarnpkg.com/lang/en/) (or [npm](https://www.npmjs.com/))
* Optional: [`Pulp`](https://github.com/purescript-contrib/pulp) (for running [`psci`](https://github.com/purescript/documentation/blob/master/guides/PSCi.md))


## Installation

1. Get all sources:
```sh
git clone git@github.com:aelve/guide.git
cd guide
git checkout front-ps
```

2. Get data of `guide-database`
```sh
./official.sh
```

3. Build back-end
```
./b
```

4. Build PureScript types
```
stack exec guide-hs2ps
```

5. Build front-end
```sh
cd front-ps
yarn # or npm i
```

## Run

1. Run back-end
```sh
stack exec guide
```

2. Run front-end
```sh
cd front-ps
yarn start # or npm start
```

Open [`http://localhost:3333`](http://localhost:3333).

### Other `npm` scripts (all optional)

#### watch

`yarn start` or `yarn watch` will start a development server, which
hot-reloads your application when sources changes.

#### serve

`NODE_ENV=production yarn serve` builds your application and starts a
production server.

#### build

`npm run build` builds application client and server bundles.


## Directory structure

Structure based on [`Starter Pux app`](https://github.com/alexmingoia/pux-starter-app) with slightly modifications.

- `src`: Application source code.
  - `src/Guide/Config.js`: Configuration values.
  - `src/Guide/Config.purs`: Configuration type.
  - `src/Guide/Events.purs`: Application event type and foldp function.
  - `src/Guide/Routes.purs`: Routes.
  - `src/Guide/State.purs`: Application state type and init function.
  - `src/Guide/View/HTMLWrapper.purs`: HTML document view.
  - `src/Guide/View/Homepage.purs`: Home page.
  - `src/Guide/View/Layout.purs`: App layout.
  - `src/Guide/View/NotFound.purs`: 404 page.
  - `src/Generated/*`: Generated PureScript types (this folder is ignored by default).

- `static`: Static files served with application.
- `support`: Support files for building.

- `Client.purs`: Client entry point.
- `index.client.js`: Webpack entry point. Handles hot reloading.

- `Server.purs`: Server entry point.
- `index.server.js`: Webpack entry point.

- `bower.json`: Bower package configuration.
- `package.json`: Node package configuration.


- `webpack.config.client.js`: Webpack client configuration.
- `webpack.config.server.js`: Webpack server configuration.
- `server.js`: Simple `Express` server to run `app` locally
