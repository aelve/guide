## Commands

 - `dev` - Start developing environment (starts both server and setup client middleware for server).
 - `build` - Builds to /dist folder. Compiles (from ts to js) server right in dist folder, client files compiled in /dist/src.

## Deploy process
  Node version >= 11, because used fs async methods which experimental now

 - `git pull`
 - `npm i`
 - `npm run build`
 - `set NODE_ENV=production`, also you can specify port by `set PORT=%port_number%`
 - `cd dist`
 - `npm i`
 - `node run server`
 