## Commands

- `npm run dev` - Start developing environment (starts both server and setup client middleware for server).
- `npm run build` - Builds to /dist folder. Compiles (from ts to js) server right in dist folder, client files compiled in /dist/src.

## Deploy process
  Requirements: Node version should be >= 11, because of using fs async methods which experimental now.

1. `git pull`
2. `npm i`

3. Now you have a choice how to set PORT and API_URL. Either set them in front/config.js, or use export (set) PORT=%port_number% API_URL=%api_url% (for example "http://localhost:4400" quotes matter) with dev commands.

4. `npm run build`
5. `cd dist`
6. `npm i`
7. `export (set) NODE_ENV=production`
8. `node server`
