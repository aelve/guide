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
  async loadCategoryItem({ commit }: ActionContext<CategoryItemState, any>, category): Promise<any> {
    const data: ICategoryItem[] = await CategoryItemService.getCategoryItem(category)
    commit('setCategoryItem', data)
  },
  async createItem({ dispatch }, { category, name }: ICategoryItem): Promise<any> {
    const createdId = await CategoryItemService.addItem({
      category,
      name
    })
    dispatch('loadCategoryItem')
    return createdId
  },
  async deleteItem({ dispatch }, { id }: ICategoryItem) {
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