import Router from 'vue-router'

function createRouter() {
  return new Router({
    mode: 'history',
    fallback: false,
    scrollBehavior: (to) => {
      // ads an ability for a scroll to anchor
      if (to.hash) {
        return { selector: to.hash }
      } else {
        return { x:0, y:0 }
      }
    },
    routes: [
      {
        path: '/',
        name: 'Index',
        component: () => import('../page/Index.vue')
      },
      { path: '/haskell/:category',
        name: 'Category', 
        component: () => import('../page/ArticlePage.vue'),
        props: true 
      },
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
