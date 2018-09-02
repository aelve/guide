const about = {
  namespaced: true,

  state (): IAboutState {
    return {
      oldSaying: ''
    }
  },

  mutations: {
    SET_OLD_SAYING (state: IAboutState, payload: string) {
      state.oldSaying = payload
    }
  },

  getters: {
    oldSaying (state: IAboutState) {
      return state.oldSaying
    }
  },

  actions: {
    setOldSaying ({ commit }, payload: string) {
      commit('SET_OLD_SAYING', payload)
    }
  }
}

export {
  about
}

interface IAboutState {
  oldSaying: string
}
