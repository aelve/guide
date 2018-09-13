import axios from "axios";

class CategoryService {
  async  getCategoryList(): Promise<ICategory[]> {
    const { data } = await axios.get("api/categories", {})
    return data
  }
}

export interface ICategory {
  created: string
  group: string
  status: string
  title: string
  uid: string
}

const categoryServiceInstance = new CategoryService()


export { categoryServiceInstance as CategoryService }
