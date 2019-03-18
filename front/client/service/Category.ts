import axios from 'axios'
import { ICategoryItem } from './CategoryItem'

class CategoryService {
  async getCategoryById (id: ICategoryInfo['id']): Promise<ICategoryFull> {
    const { data } = await axios.get(`api/category/${id}`, {})
    return data
  }

  async getCategoryList (): Promise<ICategoryInfo[]> {
    const { data } = await axios.get('api/categories', {})
    return data
  }

  async createCategory (
    { title, group }: { title: ICategoryInfo['title'], group: ICategoryInfo['group'] }
  ): Promise<ICategoryInfo['id']> {
    const { data } = await axios.post('api/category', null, {
      params: {
        title,
        group
      }
    })
    return data
  }

  async updateCategoryInfo ({id, title, group, status, sections}) {
    const { data } = await axios.put(`api/category/${id}/info`, {
        title,
        group,
        status,
        sections
    })
    return data
  }

  async deleteCategory (id: ICategoryInfo['id']): Promise<void> {
    await axios.delete(`api/category/${id}`)
  }
}

export enum CategoryStatus {
  finished = 'CategoryFinished',
  inProgress = 'CategoryWIP',
  toBeWritten = 'CategoryStub'
}

export interface ICategoryInfo {
  id: string
  title: string
  created: string
  group: string
  status: CategoryStatus
}

export interface ICategoryFull {
  id: string
  title: string
  group: string
  status: CategoryStatus
  description: object
  items: ICategoryItem[]
}

const categoryServiceInstance = new CategoryService()

export { categoryServiceInstance as CategoryService }
