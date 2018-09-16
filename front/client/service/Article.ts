import axios from 'axios';

class ArticleService {
  async getArticle(): Promise<IArticle[]> {
    const { data } = await axios.get('api/category/sth6l9jl', {})
    console.log(data);

    return data;
  }
}

export interface IArticle {
  status: string,
  group: string,
  uid: string,
  items: [],
  title: string,
  desription: string
}

const articleServiceInstance = new ArticleService();

export { articleServiceInstance as ArticleService }