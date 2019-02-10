import Router from 'vue-router'

function createRouter () {
  return new Router({
    mode: 'history',
    fallback: false,
    // TODO update vue-router when scroll issue will be fixed
    // https://github.com/vuejs/vue-router/issues/2095
    scrollBehavior: (to, from, savedPosition) => {
      if (savedPosition) {
        return savedPosition
      } else if (to.hash) {
        return { selector: to.hash }
      } else {
        return { x: 0, y: 0 }
      }
    },
    routes: [
      {
        path: '/',
        name: 'Index',
        component: () => import('../page/Index.vue')
      },
      {
        path: '/haskell/:category',
        name: 'Category',
        component: () => import('../page/CategoryPage.vue'),
        props: (route) => ({ categoryId: route.params.category.split('#').shift().split('-').pop() })
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
