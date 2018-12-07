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
  async deleteItemById ({ dispatch }: ActionContext<ICategoryItemState, any>, id: ICategoryItem['uid']) {
    await CategoryItemService.deleteItemById(id)
    dispatch('category/reloadCategory', null, { root: true })
  },
  async addCategoryDescription (
    { dispatch }: ActionContext<ICategoryItemState, any>,
    { uid, original, modified }: {uid: string, original: string, modified: string}
  ) {
    const createdDescription = await CategoryItemService.addCategoryDescription({
      uid,
      original,
      modified
    })
    dispatch('category/reloadCategory', null, { root: true })
    return createdDescription
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
