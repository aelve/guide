import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import {
  ICategoryItem,
  CategoryItemService,
  ICreateCategoryItem,
  ICategoryItemInfo,
  ITrait
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
  },
  async updateItemSummary (
    context: ActionContext<ICategoryItemState, any>,
    { id, original, modified }: { id: ICategoryItem['uid'], original: ICategoryItem['summary'], modified: ICategoryItem['summary'] }
  ): Promise<void> {
    await CategoryItemService.updateItemSummary(id, original, modified)
  },
  async updateItemEcosystem (
    context: ActionContext<ICategoryItemState, any>,
    { id, original, modified }: { id: ICategoryItem['uid'], original: ICategoryItem['ecosystem'], modified: ICategoryItem['ecosystem'] }
  ): Promise<void> {
    await CategoryItemService.updateItemEcosystem(id, original, modified)
  },
  async updateItemNotes (
    context: ActionContext<ICategoryItemState, any>,
    { id, original, modified }: { id: ICategoryItem['uid'], original: ICategoryItem['notes'], modified: ICategoryItem['notes'] }
  ): Promise<void> {
    await CategoryItemService.updateItemNotes(id, original, modified)
  },
  async updateItemTrait (
    context: ActionContext<ICategoryItemState, any>,
    {
      itemId,
      traitId,
      original,
      modified
    }: {
      itemId: ICategoryItem['uid'],
      traitId: ITrait['uid'],
      original: string,
      modified: string
    }
  ): Promise<void> {
    await CategoryItemService.updateItemTrait(itemId, traitId, original, modified)
  },
  async moveItemTrait (
    context: ActionContext<ICategoryItemState, any>,
    {
      itemId,
      traitId,
      direction
    }: {
      itemId: ICategoryItem['uid'],
      traitId: ITrait['uid'],
      direction: string
    }
  ): Promise<void> {
    await CategoryItemService.moveItemTrait(itemId, traitId, direction)
  },
  async deleteItemTrait (
    context: ActionContext<ICategoryItemState, any>,
    {
      itemId,
      traitId,
    }: {
      itemId: ICategoryItem['uid'],
      traitId: ITrait['uid']
    }
  ): Promise<void> {
    await CategoryItemService.deleteItemTrait(itemId, traitId)
  },
  async createItemTrait (
    context: ActionContext<ICategoryItemState, any>,
    { itemId,
      type,
      text
    }: {
      itemId: ICategoryItem['uid'],
      type: string,
      text: string,
    }
  ): Promise<void> {
    await CategoryItemService.createItemTrait(itemId, type, text)
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
