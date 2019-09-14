import api from 'client/api'

class WikiService {
  async search (searchString: string) {
    return api.get('search', {
      params: {
        query: searchString
      },
      requestName: 'search'
    })
  }
}

const wikiServiceInstance = new WikiService()

export { wikiServiceInstance as WikiService }
