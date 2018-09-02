# @vert/vue-ssr-template

> Vue project template with SSR, Vert and TypeScript support.

A tiny demo to show how to use [@vert/core](https://github.com/LancerComet/Vert-Core).

You can use this as your vert template.

## Features
 - Webpack 4.
 - Vue SSR support.
 - Full TypeScript.
 - Code in OOP.
 - [@vert/core](https://github.com/LancerComet/Vert-Core) support.

## Commands

 - `client:dev` - Start developing environment of client content.
 - `client:build` - Build client content.
 - `server:dev` - Start developing environment of ssr server. 
 - `server:build` - Build ssr content.
 - `build-all` - Run `client:build` and `server:build` at same time. Please run this command for deployment.
 - `start:dev` - Run `client:dev` and `server:dev` at same time. Please run this command for development.
 - `start:prod` - Start ssr server in production environment. You should run `build-all` first to make it works. 

## Development

Normally just run `start:dev` to start both normal http and ssr environment, and then:

 - Go `localhost:4000` for normal http environment.
 - Go `localhost:5000` for SSR-enabled environment.
 
Ports setting is stored in `build/build-config.js`.

## Production

Run `build-all` to files for production.

Then,

 - Send your static files which are located in `dist/` to your (CDN) server.
 - If you want to host a SSR server, the simplest way is to send the whole project to somewhere and run `start:prod`. But you can also build service by your own, just transfer vue-bundle-json files under `dist/` to your location and setup your API Gateway. 
