import Vuex from 'vuex'
import category from 'client/store/modules/category'
import categoryItem from 'client/store/modules/categoryItem'
import wiki from 'client/store/modules/wiki'

function createStore () {
  return new Vuex.Store({
    state: {
      isPageLoading: false,
      is404: false
    },
    actions: {},
    mutations: {
      tooglePageLoading (state) {
        state.isPageLoading = !state.isPageLoading
      },
      set404 (state, val) {
        state.is404 = val
      }
    },
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
