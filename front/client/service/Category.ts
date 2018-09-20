import axios from "axios";

class CategoryService {
  async  getCategoryList(): Promise<ICategory[]> {
    const { data } = await axios.get("api/categories", {})
    return data
  }

  async  createCategory({ title }: ICategory): Promise<ICategory[]> {
    const { data } = await axios.post('api/category', null, {
      params: {
        title
      }
    })
    return data
  }

  async getCategoryItem(): Promise<ICategory[]> {
    const { data } = await axios.get('api/category/sth6l9jl', {})
    console.log(data.items);

    return data;
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
  items: any[]
}


const categoryServiceInstance = new CategoryService()


export { categoryServiceInstance as CategoryService }
