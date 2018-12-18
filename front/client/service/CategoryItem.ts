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

  // add here category description add/edit
  async addCategoryDescription ({ uid, original, modified }: {uid: string, original: string, modified: string}): Promise<any> {
    // try {
    //   const { data } = await axios.put(`api/category/${uid}/notes`, {
    //       original,
    //       modified
    //   })
    //   return data
    // } catch(err) {
    //   if (err.response.status === '409') {
    //     console.log(409 + ' blyat')
    //   }
    //   throw err
    // }
    const { data } = await axios.put(`api/category/${uid}/notes`, {
      original,
      modified
    })
    return data
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
