import axios from "axios";
class WikiService {
  async search(searchString: string) {
    const { data } = await axios.get('api/search', {
      params: {
        query: searchString
      }
    })
    return data
  }
}


const wikiServiceInstance = new WikiService()


export { wikiServiceInstance as WikiService }
