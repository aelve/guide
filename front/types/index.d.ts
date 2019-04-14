declare module '*.vue' {
  const content: any
  export default content
}

declare module '*/config.js' {
  const content: any
  export default content
}

declare module 'koa-proxy'