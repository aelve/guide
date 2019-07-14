import axios from 'axios'
import { ICategoryFull } from './Category'

class CategoryItemService {
  // TODO replace all axios /api request to axios instance to remove duplication of '/api/*'
  async createItem (
    {
      category,
      name,
      hackage,
      link
    }: ICreateCategoryItem) {
    const { data } = await axios.post(`/api/item/${category}`, {
      name,
      hackage,
      link
    })
    return data
  }
  async deleteItemById (id: ICategoryItem['id']): Promise<void> {
    await axios.delete(`/api/item/${id}`)
  }
  async  updateItemInfo (id: ICategoryItem['id'], body: ICategoryItemInfo): Promise<void> {
    console.log('updateItemInfo body', body)
    await axios.put(`/api/item/${id}/info`, body)
  }
  async moveItem (id: ICategoryItem['id'], direction: string): Promise<void> {
    await axios.post(`/api/item/${id}/move`, {
      direction
    })
  }
  async  updateItemSummary (
    id: ICategoryItem['id'],
    original: ICategoryItem['summary'],
    modified: ICategoryItem['summary']
  ): Promise<void> {
    await axios.put(`/api/item/${id}/summary`, {
      original,
      modified
    })
  }
  async updateItemEcosystem (
    id: ICategoryItem['id'],
    original: ICategoryItem['ecosystem'],
    modified: ICategoryItem['ecosystem']
  ): Promise<void> {
    await axios.put(`/api/item/${id}/ecosystem`, {
      original,
      modified
    })
  }
  async updateItemNotes (
    id: ICategoryItem['id'],
    original: ICategoryItem['notes'],
    modified: ICategoryItem['notes']
  ): Promise<void> {
    await axios.put(`/api/item/${id}/notes`, {
      original,
      modified
    })
  }
  async updateItemTrait (
    itemId: ICategoryItem['id'],
    traitId: ITrait['id'],
    original: string,
    modified: string
  ): Promise<void> {
    await axios.put(`/api/item/${itemId}/trait/${traitId}`, {
      original,
      modified
    })
  }
  async moveItemTrait (
    itemId: ICategoryItem['id'],
    traitId: ITrait['id'],
    direction: string
  ): Promise<void> {
    await axios.post(`/api/item/${itemId}/trait/${traitId}/move`, {
      direction
    })
  }
  async deleteItemTrait (
    itemId: ICategoryItem['id'],
    traitId: ITrait['id'],
  ): Promise<void> {
    await axios.delete(`/api/item/${itemId}/trait/${traitId}`)
  }
  async createItemTrait (
    itemId: ICategoryItem['id'],
    type: string,
    content: string,
  ): Promise<void> {
    await axios.post(`/api/item/${itemId}/trait/`, {
      type,
      content
    })
  }
  // add here category description add/edit
  async updateCategoryDescription ({ id, original, modified }: { id: string, original: string, modified: string }): Promise<any> {
    const { data } = await axios.put(`/api/category/${id}/notes`, {
      original,
      modified
    })
    return data
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
  group?: string
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
