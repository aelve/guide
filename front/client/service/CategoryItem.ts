import axios from 'axios'
import { awaitExpression } from 'babel-types';

class CategoryItemService {
  async getCategoryItem(): Promise<ICategoryItem[]> {
    const { data } = await axios.get('api/category/sth6l9jl', {})
    return data
  }

<<<<<<< HEAD
  async addItem({ category, name }: ICategoryItem['items']) {
=======
  async addItem({ category, name }: ICategoryItem) {
>>>>>>> 2b47740b86f9098a71f633c54b844288c98479fb
    const { data } = await axios.post('api/item/sth6l9jl', null, {
      params: {
        category,
        name
      }
    })
    return data
  }
  
<<<<<<< HEAD
  async deleteItem({ id }: ICategoryItem['items']) {
=======
  async deleteItem({ id }: ICategoryItem) {
>>>>>>> 2b47740b86f9098a71f633c54b844288c98479fb
    const { data } = await axios.delete(`api/item/${id}`)
    return data
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