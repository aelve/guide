import _toKebabCase from 'lodash/kebabCase'
import { ICategoryInfo } from 'client/service/Category'

interface IGetCategoryUrl {
  title: ICategoryInfo['title']
  id: ICategoryInfo['id']
}

export default function getCategoryUrl ({ title, id }: IGetCategoryUrl): string {
  return `${_toKebabCase(title)}-${id}`
}
