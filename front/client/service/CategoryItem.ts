import axios from 'axios'

class CategoryItemService {
  async getCategoryItem(categoryURL): Promise<ICategoryItem[]> {
    const { data } = await axios.get(`api/category/${categoryURL}`, {})
    return data
  }

  async addItem(categoryURL, { category, name }: ICategoryItem) {
    const { data } = await axios.post(`api/item/${categoryURL}`, null, {
      params: {
        category,
        name
      }
    })
    return data
  }
  
  async deleteItem({ id }: ICategoryItem) {
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