export default async function handler (ctx, renderer) {
  const { url } = ctx

  try {
    ctx.response.header['Content-Type'] = 'text/html'
    ctx.body = await renderer.renderToString({ url })
  } catch (error) {
    const is404 = error.response && error.response.status === 404
    if (is404) {
      ctx.body = await renderer.renderToString({ url, is404 })
    } else {
      console.error('[Error] SSR render error:', error)
      ctx.status = 500
      ctx.body = error.message || 'Unknown Internal Server Error'
    }
  }
}
