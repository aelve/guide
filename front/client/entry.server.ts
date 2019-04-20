import _get from 'lodash/get'
import axios from 'axios'
import { createApp } from './app'

import config from '../config'

axios.defaults.baseURL = `http://localhost:${config.port}`

export default async context => {
  return new Promise((resolve, reject) => {
    const { app, router, store } = createApp()

    router.push(context.url)
    router.onReady(() => {
      const matchedComponents = router.getMatchedComponents()

      // TODO not reject, create fallback to 404 component
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
