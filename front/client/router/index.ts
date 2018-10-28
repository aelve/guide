import Router from 'vue-router'

function createRouter() {
  return new Router({
    mode: 'history',
    fallback: false,
    scrollBehavior: () => ({ x: 0, y: 0 }),
    routes: [
      {
        path: '/',
        name: 'Index',
        component: () => import('../page/Index.vue')
      },
      { path: '/haskell', component: () => import('../page/ArticlePage.vue') },
      {
        path: '/haskell/search/results/',
        name: 'SearchResults',
        component: () => import('../page/SearchResults.vue'),
        props: (route) => ({ query: route.query.query })
      },
    ]
  })
}

export {
  createRouter
}
