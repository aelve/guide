import Vuex from 'vuex'
import category from 'client/store/modules/category'
import categoryItem from 'client/store/modules/categoryItem'

function createStore() {
  // TODO loggins mutations in dev
  return new Vuex.Store({
    state: {},
    actions: {},
    mutations: {},
    modules: {
      category,
      categoryItem
    }
  })
}

export {
  createStore
}
