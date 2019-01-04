import axios from 'axios'
import { ICategoryFull } from './Category'

class CategoryItemService {
  // TODO replace all axios api request to axios instance to remove duplication of 'api/*'
  async createItem ({ category, name }: ICreateCategoryItem) {
    const { data } = await axios.post(`api/item/${category}`, null, {
      params: {
        name
      }
    })
    return data
  }
  async deleteItemById (id: ICategoryItem['uid']): Promise<void> {
    await axios.delete(`api/item/${id}`)
  }
  async  updateItemInfo (id: ICategoryItem['uid'], body: ICategoryItemInfo): Promise<void> {
    await axios.put(`api/item/${id}/info`, body)
  }
  async  updateItemDescription (
    id: ICategoryItem['uid'],
    original: ICategoryItem['description'],
    modified: ICategoryItem['description']
  ): Promise<void> {
    await axios.put(`api/item/${id}/summary`, {
      original,
      modified
    })
  }
  async updateItemEcosystem (
    id: ICategoryItem['uid'],
    original: ICategoryItem['ecosystem'],
    modified: ICategoryItem['ecosystem']
  ): Promise<void> {
    await axios.put(`api/item/${id}/ecosystem`, {
      original,
      modified
    })
  }
  async updateItemNotes (
    id: ICategoryItem['uid'],
    original: ICategoryItem['notes'],
    modified: ICategoryItem['notes']
  ): Promise<void> {
    await axios.put(`api/item/${id}/notes`, {
      original,
      modified
    })
  }
  async updateItemTrait (
    itemId: ICategoryItem['uid'],
    traitId: ITrait['uid'],
    original: string,
    modified: string
  ): Promise<void> {
    await axios.put(`api/item/${itemId}/trait/${traitId}`, {
      original,
      modified
    })
  }
  async moveItemTrait (
    itemId: ICategoryItem['uid'],
    traitId: ITrait['uid'],
    direction: string
  ): Promise<void> {
    await axios.post(`api/move/item/${itemId}/trait/${traitId}`, {
      direction
    })
  }
  async deleteItemTrait (
    itemId: ICategoryItem['uid'],
    traitId: ITrait['uid'],
  ): Promise<void> {
    await axios.delete(`api/item/${itemId}/trait/${traitId}`)
  }
  async createItemTrait (
    itemId: ICategoryItem['uid'],
    type: string,
    text: string,
  ): Promise<void> {
    await axios.post(`api/item/${itemId}/trait/${type}`, JSON.stringify(text), {
      headers: {
        'Content-Type': 'application/json;charset=utf-8'
      }
    })
  }
}

export interface ICreateCategoryItem {
  category: ICategoryFull['title'],
  name: ICategoryItem['name']
}

export interface ICategoryItem {
  uid: string
  name: string
  created: string
  group?: string
  // TODO add appropriate types for description, ecosystem and other properties with structure like
  // { text: string, html: string }
  description: object
  pros: ITrait[]
  cons: ITrait[]
  ecosystem: object
  notes: object
  link?: string
  kind: object

}

export interface ICategoryItemInfo {

  uid?: string
  name?: string
  created?: string
  link?: string
  kind?: object
}

export interface ITrait {
  uid: string
  content: object
}

const catItemServiceInstance = new CategoryItemService()

export { catItemServiceInstance as CategoryItemService }
