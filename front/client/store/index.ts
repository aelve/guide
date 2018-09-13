import Vuex from 'vuex'
import category from 'client/store/modules/category'

function createStore() {
  // TODO loggins mutations in dev
  return new Vuex.Store({
    state: {},
    actions: {},
    mutations: {},
    modules: {
      category
    }
  })
}

export {
  createStore
}
