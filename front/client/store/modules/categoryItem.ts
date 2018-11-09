import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { ICategoryItem, CategoryItemService } from 'client/service/CategoryItem'

interface CategoryItemState {
  categoryItemList: ICategoryItem[]
}

const state: CategoryItemState = {
  categoryItemList: []
}

const getters: GetterTree<CategoryItemState, any> = {}

const actions: ActionTree<CategoryItemState, any> = {
  async loadCategoryItem({ commit }: ActionContext<CategoryItemState, any>): Promise<any> {
    const data: ICategoryItem[] = await CategoryItemService.getCategoryItem()
    commit('setCategoryItem', data)
  },
<<<<<<< HEAD
  async createItem({ dispatch }, { category, name }: ICategoryItem['items']) {
=======
  async createItem({ dispatch }, { category, name }: ICategoryItem): Promise<any> {
>>>>>>> 2b47740b86f9098a71f633c54b844288c98479fb
    const createdId = await CategoryItemService.addItem({
      category,
      name
    })
    dispatch('loadCategoryItem')
    return createdId
  },
<<<<<<< HEAD
  async deleteItem({ dispatch }, { id }: ICategoryItem['items']) {
=======
  async deleteItem({ dispatch }, { id }: ICategoryItem) {
>>>>>>> 2b47740b86f9098a71f633c54b844288c98479fb
    const deletedId = await CategoryItemService.deleteItem({
      id
    })
    dispatch('loadCategoryItem')
    return deletedId
  }
}

const mutations: MutationTree<CategoryItemState> = {
  setCategoryItem: (state: CategoryItemState, payload: ICategoryItem[]) => {
    state.categoryItemList = payload
  }
}

const categoryItem: Module<CategoryItemState, any> = {
  namespaced: true,
  state,
  getters,
  actions,
  mutations
}

export default categoryItem