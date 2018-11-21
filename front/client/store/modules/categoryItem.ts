import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { ICategoryItem, CategoryItemService, ICreateCategoryItem } from 'client/service/CategoryItem'
import { CategoryService, ICategoryFull } from 'client/service/Category'

interface ICategoryItemState {
  categoryItemList: ICategoryItem[]
}

const state: ICategoryItemState = {
  categoryItemList: []
}

const getters: GetterTree<ICategoryItemState, any> = {}

const actions: ActionTree<ICategoryItemState, any> = {
  async loadCategoryItem ({ commit }: ActionContext<ICategoryItemState, any>): Promise<void> {
    const data: ICategoryFull[] = await CategoryService.getCategoryById()
    commit('setCategoryItem', data)
  },
  async createItem (
    { dispatch }: ActionContext<ICategoryItemState, any>,
    { category, name }: ICreateCategoryItem
  ): Promise<ICategoryItem['uid']> {
    const createdId = await CategoryItemService.addItem({
      category,
      name
    })
    dispatch('loadCategoryItem')
    return createdId
  },
  async deleteItemById ({ dispatch }: ActionContext<ICategoryItemState, any>, id: ICategoryItem['uid']) {
    const deletedId = await CategoryItemService.deleteItemById(id)
    dispatch('loadCategoryItem')
    return deletedId
  }
}

const mutations: MutationTree<ICategoryItemState> = {
  setCategoryItem: (state: ICategoryItemState, payload: ICategoryItem[]) => {
    state.categoryItemList = payload
  }
}

const categoryItem: Module<ICategoryItemState, any> = {
  namespaced: true,
  state,
  getters,
  actions,
  mutations
}

export default categoryItem
