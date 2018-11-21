import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { ICategoryInfo, CategoryService } from 'client/service/Category'

interface ICategoryState {
  categoryList: ICategoryInfo[]
}

const state: ICategoryState = {
  categoryList: []
}

const getters: GetterTree<ICategoryState, any> = {}

const actions: ActionTree<ICategoryState, any> = {
  async loadCategoryList ({ commit }: ActionContext<ICategoryState, any>): Promise<any> {
    const data: ICategoryInfo[] = await CategoryService.getCategoryList()
    commit('setCategoryList', data)
  },
  async createCategory (
    { dispatch }: ActionContext<ICategoryState, any>,
    { title, group }: { title: ICategoryInfo['title'], group: ICategoryInfo['group'] }
  ): Promise<ICategoryInfo['uid']> {
    const createdId = await CategoryService.createCategory({
      title,
      group
    })
    dispatch('loadCategoryList')
    return createdId
  }
}

const mutations: MutationTree<ICategoryState> = {
  setCategoryList: (state: ICategoryState, payload: ICategoryInfo[]) => {
    state.categoryList = payload
  }
}

const category: Module<ICategoryState, any> = {
  namespaced: true,
  state,
  getters,
  actions,
  mutations
};

export default category
