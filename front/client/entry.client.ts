import Vue from 'vue'
import 'reflect-metadata'
import 'babel-polyfill'
import _get from 'lodash/get'

import { createApp } from './app'

const { app, router, store } = createApp()

const STATE_KEY = '__INITIAL_STATE__'
const SSR_ENABLED = window['__SSR_IS_ON__']

if (window[STATE_KEY]) {
  store.replaceState(window[STATE_KEY])
}

if (!SSR_ENABLED) {
  registerBeforeResolve()
}

router.onReady(() => {
  // TODO get rid of SSR_ENABLED constant
  if (SSR_ENABLED) {
    registerBeforeResolve()
  }
  app.$mount('#app')
})

function registerBeforeResolve() {
  router.afterEach(async (to, from) => {
    Vue.nextTick(() => {
      for (const matchedRoute of to.matched) {
        const componentsInstances = Object.values(matchedRoute.instances)
          .filter(Boolean)
        const matchedComponentsAndChildren = componentsInstances
          .reduce((acc, matchedComponent) => {
            const componentAndItsChildren = getComponentAndItsChildren(matchedComponent)
            acc = acc.concat(componentAndItsChildren)
            return acc
          }, [])
        matchedComponentsAndChildren.map(component => {
          if (typeof component.asyncData === 'function') {
            return component.$nextTick(() => component.asyncData())
          }
        })
      }
    })
  })
}

function getComponentAndItsChildren(component, result?) {
  if (!result) {
    result = []
  }
  if (!result.includes(component)) {
    result.push(component)
  }
  const children = Object.values(component.$children)
    // Parent component is also presents in components object
    .filter(x => x !== component)
  children.forEach(x => getComponentAndItsChildren(x, result))

  return result
}