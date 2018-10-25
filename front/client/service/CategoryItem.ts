import axios from 'axios'

class CategoryItemService {
  async getCategoryItem(): Promise<ICategoryItem[]> {
    const { data } = await axios.get('api/category/sth6l9jl', {})
<<<<<<< HEAD
    return data
=======

    return data;
>>>>>>> 5eebd34a28521fdfe0a4d9f7a90ad22fb32e5c15
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