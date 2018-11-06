import axios from 'axios'
import { awaitExpression } from 'babel-types';

class CategoryItemService {
  async getCategoryItem(): Promise<ICategoryItem[]> {
    const { data } = await axios.get('api/category/sth6l9jl', {})
    return data
<<<<<<< HEAD
  }

  async addItem({ category, name }: ICategoryItem['items']) {
    const { data } = await axios.post('api/item/sth6l9jl', null, {
      params: {
        category,
        name
      }
    })
    return data
  }
  
  async deleteItem({ id }: ICategoryItem['items']) {
    const { data } = await axios.delete(`api/item/${id}`)
    return data
=======
>>>>>>> e76bfbd1ab06f5833c7d7480b1ed6c8d6f06a022
  }
}

export interface ICategoryItem {
  status?: string,
  group?: string,
  uid?: string,
  items?: any[],
  title?: string,
  description?: object
}

const catItemServiceInstance = new CategoryItemService()

export { catItemServiceInstance as CategoryItemService }