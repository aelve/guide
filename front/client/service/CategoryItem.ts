import api from 'client/api'
import { ICategoryFull } from './Category'

class CategoryItemService {
  async createItem (
    {
      category,
      name,
      hackage,
      link
    }: ICreateCategoryItem) {
    return api.post(`item/${category}`, {
      name,
      hackage,
      link
    }, { requestName: 'create item' })
  }
  async deleteItemById (id: ICategoryItem['id']): Promise<void> {
    await api.delete(`item/${id}`, { requestName: 'delete item' })
  }
  async  updateItemInfo (id: ICategoryItem['id'], body: ICategoryItemInfo): Promise<void> {
    await api.put(`item/${id}/info`, body, { requestName: 'update item info' })
  }
  async moveItem (id: ICategoryItem['id'], direction: string): Promise<void> {
    await api.post(`item/${id}/move`, {
      direction
    }, { requestName: 'move item' })
  }
  async  updateItemSummary (
    id: ICategoryItem['id'],
    original: ICategoryItem['summary'],
    modified: ICategoryItem['summary']
  ): Promise<void> {
    await api.put(`item/${id}/summary`, {
      original,
      modified
    }, { requestName: 'update item summary', skipErrorCodes: [409] })
  }
  async updateItemEcosystem (
    id: ICategoryItem['id'],
    original: ICategoryItem['ecosystem'],
    modified: ICategoryItem['ecosystem']
  ): Promise<void> {
    await api.put(`item/${id}/ecosystem`, {
      original,
      modified
    }, { requestName: 'update item ecosystem', skipErrorCodes: [409] })
  }
  async updateItemNotes (
    id: ICategoryItem['id'],
    original: ICategoryItem['notes'],
    modified: ICategoryItem['notes']
  ): Promise<void> {
    await api.put(`item/${id}/notes`, {
      original,
      modified
    }, { requestName: 'update item notes', skipErrorCodes: [409] })
  }
  async updateItemTrait (
    itemId: ICategoryItem['id'],
    traitId: ITrait['id'],
    original: string,
    modified: string
  ): Promise<void> {
    await api.put(`item/${itemId}/trait/${traitId}`, {
      original,
      modified
    }, { requestName: 'update item trait', skipErrorCodes: [409] })
  }
  async moveItemTrait (
    itemId: ICategoryItem['id'],
    traitId: ITrait['id'],
    direction: string
  ): Promise<void> {
    await api.post(`item/${itemId}/trait/${traitId}/move`, {
      direction
    }, { requestName: 'move item trait' })
  }
  async deleteItemTrait (
    itemId: ICategoryItem['id'],
    traitId: ITrait['id'],
  ): Promise<void> {
    await api.delete(`item/${itemId}/trait/${traitId}`, { requestName: 'delete item trait' })
  }
  async createItemTrait (
    itemId: ICategoryItem['id'],
    type: string,
    content: string,
  ): Promise<void> {
    await api.post(`item/${itemId}/trait/`, {
      type,
      content
    }, { requestName: 'create item trait' })
  }
}

export interface ICreateCategoryItem {
  category: ICategoryFull['title'],
  name: ICategoryItem['name'],
  hackage: ICategoryItem['hackage'],
  link: ICategoryItem['link']
}

export interface ICategoryItem {
  id: string
  name: string
  created: string
  // TODO add appropriate types for summary, ecosystem and other properties with structure like
  // { text: string, html: string }
  summary: object
  pros: ITrait[]
  cons: ITrait[]
  ecosystem: object
  notes: object
  link?: string
  hackage: string
}

export interface ICategoryItemInfo {
  id?: string
  name?: string
  created?: string
  link?: string
  hackage?: string
}

export interface ITrait {
  id: string
  content: object
}

const catItemServiceInstance = new CategoryItemService()

export { catItemServiceInstance as CategoryItemService }
