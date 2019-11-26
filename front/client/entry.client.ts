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
  app.$mount('#app')
})
