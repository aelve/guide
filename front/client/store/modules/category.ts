import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { ICategoryInfo, ICategoryFull, CategoryService } from 'client/service/Category'

interface ICategoryState {
  categoryList: ICategoryInfo[],
  category: ICategoryFull
}

const state = (): ICategoryState => ({
  categoryList: [],
  category: null
})

const getters: GetterTree<ICategoryState, any> = {}

const actions: ActionTree<ICategoryState, any> = {
  async reloadCategory ({ dispatch, state }: ActionContext<ICategoryState, any>) {
    const category = state.category
    if (!category) {
      return
    }
    await dispatch('loadCategory', category.id)
  },
  async loadCategory (
    { commit }: ActionContext<ICategoryState, any>,
    categoryId: ICategoryInfo['id']
  ): Promise<any> {
    const data: ICategoryFull = await CategoryService.getCategoryById(categoryId)
    // TODO create set function for all the store
    commit('setCategory', data)
  },
  async loadCategoryList ({ commit }: ActionContext<ICategoryState, any>): Promise<any> {
    const data: ICategoryInfo[] = await CategoryService.getCategoryList()
    commit('setCategoryList', data)
  },
  async createCategory (
    { dispatch }: ActionContext<ICategoryState, any>,
    { title, group }: { title: ICategoryInfo['title'], group: ICategoryInfo['group'] }
  ): Promise<ICategoryInfo['id']> {
    const createdId = await CategoryService.createCategory({
      title,
      group
    })
    dispatch('loadCategoryList')
    return createdId
  },
  async updateCategoryInfo (context: ActionContext<ICategoryState, any>, { id, title, group, status, sections }) {
    await CategoryService.updateCategoryInfo({ id, title, group, status, sections })
  },
  async deleteCategory (
    { dispatch }: ActionContext<ICategoryState, any>,
    id: ICategoryInfo['id']
  ): Promise<void> {
    await CategoryService.deleteCategory(id)
  }
}

const mutations: MutationTree<ICategoryState> = {
  setCategoryList: (state: ICategoryState, payload: ICategoryInfo[]) => {
    state.categoryList = payload
  },
  setCategory: (state: ICategoryState, payload: ICategoryFull) => {
    state.category = payload
  }
}

const category: Module<ICategoryState, any> = {
  namespaced: true,
  state,
  getters,
  actions,
  mutations
}

export default category
