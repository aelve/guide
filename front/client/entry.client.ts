import Vue from 'vue'
import 'reflect-metadata'
import 'babel-polyfill'
import _get from 'lodash/get'

import { createApp } from './app'

const { app, router, store } = createApp()

const STATE_KEY = '__INITIAL_STATE__'

if (window[STATE_KEY]) {
  store.replaceState(window[STATE_KEY])
}

router.onReady(() => {
  registerBeforeResolve()
  app.$mount('#app')
})

function registerBeforeResolve () {
  router.beforeEach(async (to, from, next) => {
    // This case handles navigation to anchors on same page
    if (to.path === from.path) {
      next()
      return
    }

    store.commit('tooglePageLoading')
    if (!to.matched.length) {
      store.commit('tooglePageLoading')
      next()
      return
    }
    const propsOption = to.matched[0].props.default
    const props = propsOption
      ? typeof propsOption === 'function'
        ? propsOption(to)
        : typeof propsOption === 'object'
          ? propsOption
          : to.params
      : {}
    const routeComponent = to.matched[0].components.default
    const matchedRootComponent = routeComponent.cid // Check if component already imported
      ? routeComponent
      : (await routeComponent()).default
    const matchedComponentsAndChildren = getComponentAndItsChildren(matchedRootComponent)
    await Promise.all(matchedComponentsAndChildren.map(component => {
      const asyncData = component.options.methods.asyncData
      if (typeof asyncData === 'function') {
        return asyncData.call({
          $store: store,
          $router: router,
          ...props
        })
      }
    }))
    store.commit('tooglePageLoading')
    next()
  })
}

function getComponentAndItsChildren (component, result?) {
  if (!result) {
    result = []
  }
  if (!result.includes(component)) {
    result.push(component)
  }
  const children = Object.values(component.options.components)
    // Parent component is also presents in components object
    .filter(x => x !== component)
  children.forEach(x => getComponentAndItsChildren(x, result))

  return result
}
