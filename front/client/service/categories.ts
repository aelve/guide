import axios from "axios";

class CategoryService {
  async getCategoriesList(): Promise<ICategory[]> {
    const { data } = await axios.get("api/categories")
    return data
  }
}

interface ICategory {
  created: string
  group: string
  status: string
  title: string
  uid: string
}

export {
  CategoryService,
  ICategory
}
