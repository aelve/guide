import Vuex from 'vuex'
import category from 'client/store/modules/category'
import categoryItem from 'client/store/modules/categoryItem'
import wiki from 'client/store/modules/wiki'

function createStore () {
  // TODO loggins mutations in dev
  return new Vuex.Store({
    state: {},
    actions: {},
    mutations: {},
    modules: {
      category,
      categoryItem,
      wiki
    }
  })
}

export {
  createStore
}
