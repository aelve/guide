import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { ICategory, CategoryService } from 'client/service/Category'

interface CategoryState {
  categoryList: ICategory[]
}

const state: CategoryState = {
  categoryList: []
}

const getters: GetterTree<CategoryState, any> = {}

const actions: ActionTree<CategoryState, any> = {
  async loadCategoryList({ commit }: ActionContext<CategoryState, any>): Promise<any> {
    const data: ICategory[] = await CategoryService.getCategoryList()
    commit('setCategoryList', data)
  },

  async loadCategoryItem({ commit }: ActionContext<CategoryState, any>): Promise<any> {
    const data: ICategory[] = await CategoryService.getCategoryItem()
    commit('setCategoryItem', data)
  }
}

const mutations: MutationTree<CategoryState> = {
  setCategoryList: (state: CategoryState, payload: ICategory[]) => {
    state.categoryList = payload
  }
}

const category: Module<CategoryState, any> = {
  namespaced: true,
  state,
  getters,
  actions,
  mutations
};

export default category