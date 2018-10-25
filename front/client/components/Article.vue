<template>
  <v-container>
    <div class="article-wrapper">
      <div class="article-top">
        <i class="fas fa-rss"></i>
        <div class="article-top-data" v-for="(value, key) in getCategory" :key="key">
          <a class="article-top-link" href="https://guide.aelve.com/haskell/lenses-sth6l9jl">{{value.title}}</a>
          <p class="article-top-group">{{value.group}}</p>
        </div>
      </div>
      <div v-if="categoryDescription !== ''" class="article-description">
        <div v-html="categoryDescription"></div>
      </div>
      <div v-for="(value, index) in getCategoryItems" :key="index">
        <article-content 
          :kind = "value.kind.contents" 
          :group="value.group" 
          :itemDescription="value.description.html"
          :pros="value.pros"
          :cons="value.cons"
          :ecosystem="value.ecosystem.html"
          :tocArray="value.toc"
          :notes="value.notes.html"
        />
      </div>
    </div>
  </v-container>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import ArticleContent from 'client/components/ArticleContent.vue'

@Component({
  components: {
    ArticleContent
  }
})
export default class ArticleItem extends Vue {
  // description: boolean = false
  categoryDescription: string = ''

  async asyncData() {
    return this.$store.dispatch('categoryItem/loadCategoryItem').then(() => {
      this.categoryDescription = this.$store.state.categoryItem.categoryItemList.description.html
    })
  }

  get getCategory () {
    return this.$store.state.categoryItem
  }

  get getCategoryItems() {
    return this.$store.state.categoryItem.categoryItemList.items
  }

  created() {
    this.asyncData()
  }
}
</script>

<style scoped>
  .article-top {
    display: flex;
    align-items: center;
    margin: 0 0 45px;
  }

  .article-top-data {
    display: flex;
    align-items: center;
    flex: 1;
  }

  .article-top >>> i {
    margin-right: 15px;
    font-size: 18px;
    color: #979797;
    cursor: pointer;
    transition: all ease-in-out .25s;
  }

  .article-top >>> i:hover {
    color: #000;
  }

  .article-top-link {
    font-size: 24px;
    font-weight: 600;
    text-decoration: none;
    color: #979797;
    cursor: pointer;
    transition: all ease-in-out .25s;
    margin-right: 30px;
  }

  .article-top-group {
    font-size: 24px;
  }

  .article-top-link:hover {
    color: #000;
    color: #979797;
  }

  .article-wrapper {
    width: 800px;
    margin: 0 auto;
  }

  .article-description {
    margin: 0 0 60px;
  }

  .article-description >>> p {
    font-size: 16px;
  }

  .article-description >>> h1 {
    margin: 20px 0 5px;
  }

  @media screend and (max-width: 768px) {
    .article-item {
      margin: 0 0 30px;
    }
  }

</style>
