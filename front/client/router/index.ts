import Router from 'vue-router'
import categoryPathToId from 'client/helpers/categoryPathToId'
import _get from 'lodash/get'

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
        props: (route) => ({ categoryId: categoryPathToId(route.params.category) }),
        meta: {
          documentTitle: () => {
            const category = store.state.category.category
            return category
              ? `${category.title} – Aelve Guide`
              : 'Aelve Guide'
          }
        }
      },
      {
        path: '/haskell/search/results/',
        name: 'SearchResults',
        component: () => import('../page/SearchResults.vue'),
        beforeEnter: (to, from, next) => {
          store.commit('wiki/setSearchQuery', to.query.query)
          next()
        },
        meta: {
          documentTitle: (route) => `${route.query.query} – Search results – Aelve Guide`,
        }
      },
      {
        path: '*',
        name: 'Page404',
        component: () => import('../page/Page404.vue'),
        meta: {
          documentTitle: 'Error 404 – Aelve Guide'
        }
      }
    ]
  })
}

function getRouteDocumentTitle (route) {
  const defaultTitle = 'Aelve Guide'
  const routeDocumentTitle = _get(route, 'meta.documentTitle')
  if (routeDocumentTitle) {
    return typeof routeDocumentTitle === 'function'
      ? routeDocumentTitle(route)
      : routeDocumentTitle
  } else {
    return defaultTitle
  }
}

export {
  createRouter,
  getRouteDocumentTitle
}
