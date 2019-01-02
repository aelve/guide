import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import {
  ICategoryItem,
  CategoryItemService,
  ICreateCategoryItem,
  ICategoryItemInfo
} from 'client/service/CategoryItem'

interface ICategoryItemState {
  categoryItemList: ICategoryItem[]
}

const state: ICategoryItemState = {
  categoryItemList: []
}

const getters: GetterTree<ICategoryItemState, any> = {}

const actions: ActionTree<ICategoryItemState, any> = {
  async createItem (
    { dispatch }: ActionContext<ICategoryItemState, any>,
    { category, name }: ICreateCategoryItem
  ): Promise<ICategoryItem['uid']> {
    const createdId = await CategoryItemService.createItem({
      category,
      name
    })
    dispatch('category/reloadCategory', null, { root: true })
    return createdId
  },
  async deleteItemById (context, id: ICategoryItem['uid']) {
    await CategoryItemService.deleteItemById(id)
  },
  async updateItemInfo (
    context: ActionContext<ICategoryItemState, any>,
    { id, body }: { id: ICategoryItem['uid'], body: ICategoryItemInfo }
  ): Promise<void> {
    await CategoryItemService.updateItemInfo(id, body)
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
