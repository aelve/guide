import _get from 'lodash/get'

import { createApp } from './app'

const { app, router, store } = createApp()

const STATE_KEY = '__INITIAL_STATE__'

if (window[STATE_KEY]) {
  store.replaceState(window[STATE_KEY])
}

// In case if server rendered 404 page instead of category page (or any other)
// we should manually navigate to 404 component with same url
// if its not done router will try to render category component
if (store.state.is404) {
  const currentPath = location.href.replace(location.origin, '')
  // TODO, use new api when fixed https://github.com/vuejs/vue-router/issues/977
  router.replace({ name: 'Page404', params: { '0': currentPath } })
}

router.onReady(() => {
  registerRouterHooks()
  app.$mount('#app')
})

function registerRouterHooks () {
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
    try {
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
        const serverPrefetch = component.options.serverPrefetch && component.options.serverPrefetch[0]
        if (typeof serverPrefetch === 'function') {
          return serverPrefetch.call({
            $store: store,
            $router: router,
            ...component.options.methods,
            ...props
          })
        }
      }))
      next()
    } finally {
      store.commit('tooglePageLoading')
    }
  })
}

function getComponentAndItsChildren (component, result?) {
  if (!result) {
    result = []
  }
  if (!component.options) {
    return result
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
