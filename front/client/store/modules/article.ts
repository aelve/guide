import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { IArticle, ArticleService } from 'client/service/Article'

interface ArticleState {
  articleList: IArticle[]
}

const state: ArticleState = {
  articleList: []
}

const getters: GetterTree<ArticleState, any> = {}

const actions: ActionTree<ArticleState, any> = {
  async loadArticle({commit}: ActionContext<ArticleState, any>): Promise<any> {
    const data: IArticle[] = await ArticleService.getArticle()
    commit('setArticle', data)
  }
}

const mutations: MutationTree<ArticleState> = {
  setArticle: (state: ArticleState, payload: IArticle[]) => {
    state.articleList = payload
  }
}

const article: Module<ArticleState, any> = {
  namespaced: true,
  state,
  getters,
  actions,
  mutations
}

export default article