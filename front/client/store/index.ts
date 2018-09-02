import Vuex from 'vuex'

import { about } from '../page/about/store'

function createStore () {
  return new Vuex.Store({
    modules: {
      about
    }
  })
}

export {
  createStore
}
