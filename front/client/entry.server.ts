import 'reflect-metadata'
import _get from 'lodash/get'
import { createApp } from './app'

export default async context => {
  return new Promise((resolve, reject) => {
    const { app, router, store } = createApp()

    router.push(context.url)
    router.onReady(() => {
      const matchedComponents = router.getMatchedComponents()

      if (!matchedComponents.length) {
        return reject({
          code: 404,
          error: new Error('no component matched')
        })
      }
      context.state = store.state
      resolve(app)
    }, reject)
  })
}
