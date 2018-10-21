import axios from 'axios'

class CategoryItemService {
  async getCategoryItem(): Promise<ICategoryItem[]> {
    const { data } = await axios.get('api/category/sth6l9jl', {})
    // console.log(data);
    return data;
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