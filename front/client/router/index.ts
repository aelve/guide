import Router from 'vue-router'

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
        name: 'Index',
        component: () => import('../page/Index.vue')
      },
      {
        async beforeEnter (to, from, next) {
          const categoryId = to.params.category.split('#').shift().split('-').pop()
          const goTo404 = () => next({
            name: 'Page404',
            params: { 0: to.path } // params '0' property used as path for fallback routes like "Page404"
          })
          if (!categoryId) {
            goTo404()
          }
          try {
            await store.dispatch('category/loadCategory', categoryId)
            next()
          } catch (e) {
            // if category with such id not found, replace component with 404 page
            // TODO use new api than this issue fixed https://github.com/vuejs/vue-router/issues/977
            const is404 = e.response && e.response.status === 404
            if (is404) {
              goTo404()
            } else {
              // TODO create 500 error page page
              throw e
            }
          }
        },
        path: '/haskell/:category',
        name: 'Category',
        component: () => import('../page/CategoryPage.vue'),
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
