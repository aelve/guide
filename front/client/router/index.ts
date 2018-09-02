import Router from 'vue-router'

function createRouter () {
  return new Router({
    mode: 'history',
    fallback: false,
    scrollBehavior: () => ({ x: 0, y: 0 }),
    routes: [
      { path: '/', component: () => import('../page/index/index.vue') },
      { path: '/about/:name', component: () => import('../page/about/index.vue') }
    ]
  })
}

export {
  createRouter
}
