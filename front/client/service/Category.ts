import axios from "axios";

class CategoryService {
  async  getCategoryList(): Promise<ICategory[]> {
    const { data } = await axios.get("api/categories", {})
    return data
  }

  async  createCategory({ title, group }: ICategory): Promise<ICategory['uid']> {
    const { data } = await axios.post('api/category', null, {
      params: {
        title,
        group
      }
    })
    return data
  }
}

export enum CategoryStatus {
  finished = 'CategoryFinished',
  inProgress = 'CategoryWIP',
  toBeWritten = 'CategoryStub'

}
export interface ICategory {
  created?: string
  group?: string
  status?: CategoryStatus
  title?: string
  uid?: string,
  items?: any[]
}


const categoryServiceInstance = new CategoryService()


export { categoryServiceInstance as CategoryService }
