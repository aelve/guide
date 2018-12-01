import axios from 'axios'
import { ICategoryFull } from './Category'

class CategoryItemService {
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
  description: object
  pros: ITrait[]
  cons: ITrait[]
  ecosystem: object
  notes: object
  link?: string
  kind: object

}

export interface ITrait {
  uid: string
  content: object
}

const catItemServiceInstance = new CategoryItemService()

export { catItemServiceInstance as CategoryItemService }
