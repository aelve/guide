import 'reflect-metadata'
import { createApp } from './app'

export default context => {
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

      Promise.all(matchedComponents.map((Component) => {
        const asyncDataFunc = Component['asyncData'] || (Component['options'] || {})['asyncData']
        if (typeof asyncDataFunc === 'function') {
          return asyncDataFunc({
            store,
            route: router.currentRoute
          })
        }
      })).then(() => {
        context.state = store.state
        resolve(app)
      }).catch(reject)
    }, reject)
  })
}
