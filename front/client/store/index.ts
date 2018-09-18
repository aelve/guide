import Vuex from 'vuex'
import category from 'client/store/modules/category'
import article from 'client/store/modules/article'

function createStore() {
  // TODO loggins mutations in dev
  return new Vuex.Store({
    state: {},
    actions: {},
    mutations: {},
    modules: {
      category,
      article
    }
  })
}

export {
  createStore
}
