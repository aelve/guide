import api from 'client/api'
import { ICategoryItem } from './CategoryItem'

class CategoryService {
  async getCategoryById (id: ICategoryInfo['id']): Promise<ICategoryFull> {
    return api.get(`category/${id}`, { requestName: 'get category' })
  }

  async getCategoryList (): Promise<ICategoryInfo[]> {
    return api.get('categories', { requestName: 'get category list' })
  }

  async createCategory (
    { title, group }: { title: ICategoryInfo['title'], group: ICategoryInfo['group'] }
  ): Promise<ICategoryInfo['id']> {
    return api.post('category', null, {
      params: {
        title,
        group
      },
      requestName: 'create category'
    })
  }

  async updateCategoryInfo ({ id, title, group, status, sections }) {
    return api.put(`category/${id}/info`, {
      title,
      group,
      status,
      sections
    }, { requestName: 'update category info' })
  }

  async deleteCategory (id: ICategoryInfo['id']): Promise<void> {
    await api.delete(`category/${id}`, { requestName: 'delete category' })
  }

  async updateCategoryDescription ({ id, original, modified }: { id: string, original: string, modified: string }): Promise<any> {
    return api.put(`category/${id}/notes`, {
      original,
      modified
    }, { requestName: 'update category description', skipErrorCodes: [409] })
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
