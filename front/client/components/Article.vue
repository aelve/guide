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
      <!-- <div class="article-description" v-html="getCategoryDescription"></div> -->
      <!-- TODO куда то пропал category description - разобраться -->
      <div class="article-description" v-html="getCategoryDescription"></div>
      <article-content v-bind:contentItem = "getCategoryItems" />
    </div>
  </v-container>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import ArticleContent from 'client/components/ArticleContent.vue'

// Idea 2 - вынести блок с частью статьи в отдельный компонент

@Component({
  components: {
    ArticleContent
  }
})
export default class ArticleItem extends Vue {
  // isNoteExpanded: boolean = false
  // description: string = ''
  notes: string = ''

  async asyncData() {
    return this.$store.dispatch('categoryItem/loadCategoryItem')
  }

  get getCategory () {
    // why this returns items[]?
    // return this.$store.state.categoryItem.categoryItemList
    return this.$store.state.categoryItem
  }
// TODO - Andreys changes - implement! try 3d option or completely rewrite - still bug exists
  get getCategoryDescription() {
    if (!this.$store.state.categoryItem.categoryItemList.description === undefined) {
      const description = this.$store.state.categoryItem.categoryItemList.description.html
      return description
    }
  }

  get getCategoryItems() {
    const items = this.$store.state.categoryItem.categoryItemList.items

    return items
  }

  // expandNotes() {  
  //   this.isNoteExpanded = true
  // }

  // collapseNotes() {
  //   this.isNoteExpanded = false
  // }
  // get categoryToc() {
  //   const toc = this.$store.state.categoryItem.categoryItemList
  //   console.log('-------------------------------');
    
  //   console.log(toc);
    
  //   return toc
  // } 
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

  /* TODO разобраться что это за стили */
  .article-section {
    margin: 30px 0;
  }

  .article-section.pros-cons-box, .article-section.notes-box >>> li {
    margin: 0 0 5px;
  }

  .article-section.notes-box >>> h1 {
    margin: 20px 0;
  }
  /* END TODO */
</style>
