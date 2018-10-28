import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { WikiService } from 'client/service/Wiki'

interface WikiState {
  searchResults: any[],
  searchInput: string
}

const state: WikiState = {
  searchResults: [],
  searchInput: ''
}

const getters: GetterTree<WikiState, any> = {}

const actions: ActionTree<WikiState, any> = {
  async search({ commit }: ActionContext<WikiState, any>, searchString: string): Promise<any> {
    const data: any[] = await WikiService.search(searchString)
    commit('setSearchResults', data)
  }
}

const mutations: MutationTree<WikiState> = {
  setSearchResults(state: WikiState, payload: any[]) {
    state.searchResults = payload
  },
  setSearchInput(state: WikiState, payload: string) {
    state.searchInput = payload
  }
}

const wiki: Module<WikiState, any> = {
  namespaced: true,
  state,
  getters,
  actions,
  mutations
};

export default wiki