import _get from 'lodash/get'
import { createApp } from './app'
import { getRouteDocumentTitle } from './router'

export default async context => {
  return new Promise((resolve, reject) => {
    const { app, router, store } = createApp()

    // Case when server tried to render some page but api request returned 404 and we render 404 page on same url
    if (context.is404) {
      router.push({
        name: 'Page404',
        params: { 0: context.url }
      })
    } else {
      router.push(context.url)
    }

    router.onReady(() => {
      context.rendered = () => {
        context.state = store.state
      }
      context.title = getRouteDocumentTitle(router.currentRoute)
      resolve(app)
    }, reject)
  })
}
