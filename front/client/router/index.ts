import Router from 'vue-router'

function createRouter () {
  return new Router({
    mode: 'history',
    fallback: false,
    scrollBehavior: () => ({ x: 0, y: 0 }),
    routes: [
      { path: '/', component: () => import('../page/Index.vue') },
      { path: '/haskell', component: () => import('../page/ArticlePage.vue') }
    ]
  })
}

export {
  createRouter
}
