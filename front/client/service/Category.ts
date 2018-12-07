import axios from 'axios'
import { ICategoryItem } from './CategoryItem'

class CategoryService {
  async getCategoryById (id: ICategoryInfo['uid']): Promise<ICategoryFull> {
    const { data } = await axios.get(`api/category/${id}`, {})
    return data
  }

  async getCategoryList (): Promise<ICategoryInfo[]> {
    const { data } = await axios.get('api/categories', {})
    return data
  }

  async createCategory (
    { title, group }: { title: ICategoryInfo['title'], group: ICategoryInfo['group'] }
  ): Promise<ICategoryInfo['uid']> {
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

export interface ICategoryInfo {
  uid: string
  title: string
  created: string
  group: string
  status: CategoryStatus
}

export interface ICategoryFull {
  uid: string
  title: string
  group: string
  status: CategoryStatus
  description: object
  items: ICategoryItem[]
}

const categoryServiceInstance = new CategoryService()

export { categoryServiceInstance as CategoryService }
