import { ActionTree, GetterTree, MutationTree, ActionContext, Module } from 'vuex'
import { ICategoryInfo, ICategoryFull, CategoryService } from 'client/service/Category'
import { ICategoryItem } from 'client/service/CategoryItem'

interface ICategoryState {
  categoryList: ICategoryInfo[],
  category: ICategoryFull,
  itemsSectionsInEdit: {
    ItemProsConsSection: Array<ICategoryItem['id']>
    ItemEcosystemSection: Array<ICategoryItem['id']>
    ItemNotesSection: Array<ICategoryItem['id']>
  }
}

const state = (): ICategoryState => ({
  categoryList: [],
  category: null,
  itemsSectionsInEdit: {
    ItemProsConsSection: [],
    ItemEcosystemSection: [],
    ItemNotesSection: []
  }
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
  },
  toggleItemProsConsSectionEdit (
    { commit }: ActionContext<ICategoryState, any>,
    itemId: ICategoryItem['id']
  ) {
    commit('toggleSectionEditState', { sectionName: 'ItemProsConsSection', itemId })
  },
  toggleItemEcosystemSectionEdit (
    { commit }: ActionContext<ICategoryState, any>,
    itemId: ICategoryItem['id']
  ) {
    commit('toggleSectionEditState', { sectionName: 'ItemEcosystemSection', itemId })
  },
  toggleItemNotesSectionEdit (
    { commit }: ActionContext<ICategoryState, any>,
    itemId: ICategoryItem['id']
  ) {
    commit('toggleSectionEditState', { sectionName: 'ItemNotesSection', itemId })
  },
  async updateCategoryDescription (
    { dispatch }: ActionContext<ICategoryState, any>,
    { id, original, modified }: { id: string, original: string, modified: string }
  ) {
    const createdDescription = await CategoryService.updateCategoryDescription({
      id,
      original,
      modified
    })

    return createdDescription
  }
}

const mutations: MutationTree<ICategoryState> = {
  setCategoryList: (state: ICategoryState, payload: ICategoryInfo[]) => {
    state.categoryList = payload
  },
  setCategory: (state: ICategoryState, payload: ICategoryFull) => {
    state.category = payload
  },
  toggleSectionEditState (state, { sectionName, itemId }) {
    const sectionEditState = state.itemsSectionsInEdit[sectionName]

    if (sectionEditState.includes(itemId)) {
      const index = sectionEditState.indexOf(itemId)
      sectionEditState.splice(index, 1)
    } else {
      sectionEditState.push(itemId)
    }
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
