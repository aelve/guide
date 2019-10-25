import Router from 'vue-router'
import categoryPathToId from 'client/helpers/categoryPathToId'

function createRouter (store) {
  return new Router({
    mode: 'history',
    fallback: false,
    // TODO update vue-router when scroll issue will be fixed
    // https://github.com/vuejs/vue-router/issues/2095
    // Router doesnt support navigation to same anchor yet
    // https://github.com/vuejs/vue-router/issues/1668
    scrollBehavior (to, from, savedPosition) {
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
        redirect: '/haskell'
      },
      {
        path: '/haskell',
        name: 'Index',
        component: () => import('../page/Index.vue')
      },
      {
        path: '/haskell/:category',
        name: 'Category',
        component: () => import('../page/CategoryPage.vue'),
        props: (route) => ({ categoryId: categoryPathToId(route.params.category) })
      },
      {
        path: '/haskell/search/results/',
        name: 'SearchResults',
        component: () => import('../page/SearchResults.vue'),
        props: (route) => ({ query: route.query.query })
      },
      {
        path: '*',
        name: 'Page404',
        component: () => import('../page/Page404.vue')
      }
    ]
  })
}

export {
  createRouter
}
