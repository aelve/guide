/* tslint:disable:ordered-imports */

import 'reflect-metadata'
import 'babel-polyfill'
import Vue from 'vue'

import { createApp } from './app'

const { app, router, store } = createApp()

Vue.mixin({
  beforeRouteUpdate (to, from, next) {
    const { asyncData } = this.$options
    if (typeof asyncData === 'function') {
      asyncData.call(this, {
        store: this.$store,
        route: to
      }).then(next).catch(next)
    } else {
      next()
    }
  }
})

const STATE_KEY = '__INITIAL_STATE__'
const SSR_ENABLED = window['__SSR_IS_ON__']

if (window[STATE_KEY]) {
  store.replaceState(window[STATE_KEY])
}

if (!SSR_ENABLED) {
  registerBeforeResolve()
}

router.onReady(() => {
  if (SSR_ENABLED) {
    registerBeforeResolve()
  }
  app.$mount('#app')
})

function registerBeforeResolve () {
  router.beforeResolve((to, from, next) => {
    const matched = router.getMatchedComponents(to)
    const prevMatched = router.getMatchedComponents(from)

    let diffed = false
    const activated = matched.filter((c, i) => {
      return diffed || (diffed = (prevMatched[i] !== c))
    })

    if (!activated.length) {
      return next()
    }

    Promise.all(activated.map(Component => {
      const asyncDataFunc = Component['asyncData'] ||
        (Component['options']　|| {})['asyncData']

      if (typeof asyncDataFunc === 'function') {
        return asyncDataFunc({ store, route: to })
      }
    })).then(() => {
      next()
    }).catch(next)
  })
}
