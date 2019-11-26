import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { WikiService } from 'client/service/Wiki'
import { set } from '../helpers'

interface IWikiState {
  searchResults: any[],
  searchQuery: string
}

const state = (): IWikiState => ({
  searchResults: [],
  searchQuery: ''
})

const getters: GetterTree<IWikiState, any> = {}

const actions: ActionTree<IWikiState, any> = {
  async search ({ commit }: ActionContext<IWikiState, any>, searchString: string): Promise<any> {
    const data: any[] = await WikiService.search(searchString)
    commit('setSearchResults', data)
  }
}

const mutations: MutationTree<IWikiState> = {
  setSearchResults: set('searchResults'),
  setSearchQuery: set('searchQuery')
}

const wiki: Module<IWikiState, any> = {
  namespaced: true,
  state,
  getters,
  actions,
  mutations
}

export default wiki
