import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { WikiService } from 'client/service/Wiki'

interface IWikiState {
  searchResults: any[],
  searchInput: string
}

const state: IWikiState = {
  searchResults: [],
  searchInput: ''
}

const getters: GetterTree<IWikiState, any> = {}

const actions: ActionTree<IWikiState, any> = {
  async search ({ commit }: ActionContext<IWikiState, any>, searchString: string): Promise<any> {
    const data: any[] = await WikiService.search(searchString)
    commit('setSearchResults', data)
  }
}

const mutations: MutationTree<IWikiState> = {
  setSearchResults (state: IWikiState, payload: any[]) {
    state.searchResults = payload
  },
  setSearchInput (state: IWikiState, payload: string) {
    state.searchInput = payload
  }
}

const wiki: Module<IWikiState, any> = {
  namespaced: true,
  state,
  getters,
  actions,
  mutations
}

export default wiki
