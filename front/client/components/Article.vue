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
      <!-- <div v-for="(value, index) in getCategoryItems" :key="index"
        class="article-item">
        <div class="article-header">
          <p class="article-hd-textlg">{{ value.kind.contents }}</p>
          <a-link openInNewTab :url="`http://hackage.haskell.org/package/${value.kind.contents}`" class="article-header-link">
            (Hackage)
          </a-link>
          <p class="article-hd-textsm">{{ value.group }}</p>
          <div class="article-header-icons">
            <i class="fas fa-arrow-up"></i>
            <i class="fas fa-arrow-down"></i>
            <div class="header-func-icons">
              <i class="fas fa-cogs"></i>
              <i class="fas fa-times"></i>
            </div>
          </div>
        </div>
        <div class="article-content">
          <p class="article-section-title">Summary</p>
          <div class="article-description" v-html="value.description.html"></div>
          <div class="flex-wrapper article-section pros-cons-box">
            <div class="width-50">
              <p class="article-section-title">Pros</p>
              <ul v-for="(value, index) in value.pros" :key="index">
                <li v-html="value.content.html"></li>
              </ul>
            </div>
            <div class="width-50">
              <p class="article-section-title">Cons</p>
              <ul v-for="(value, index) in value.cons" :key="index">
                <li v-html="value.content.html"></li>
              </ul>
            </div>
          </div>
          <div class="article-section">
            <p class="article-section-title">Ecosystem</p>
            <div v-html="value.ecosystem.html"></div>
          </div>
          <div class="article-section notes-box">
            <p class="article-section-title">Notes</p>
            <div class="notes-settings">
              <a href="" class="notes-settings-btn">expand notes</a>
              <a href="" class="notes-settings-btn" style="display: none;">collapse notes</a>
              <a href="" class="notes-settings-btn">edit notes</a>
            </div>
            <div v-for="(value, index) in value.toc" :key="index">
              <ul v-for="(value, index) in value" :key="index">
                <li class="notes-toc-item" v-if="value.content !== undefined">
                  <a :href="`#${value.slug}`" @click="expandNotes()">
                    <p>{{value.content.html}}</p>
                  </a>
                </li>
              </ul>
            </div>
            <div
              class="notes-content" 
              v-bind:class="{ open: isNoteExpanded }" 
              v-html="value.notes.html">
            </div>
          </div>
        </div>
      </div> -->
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

  .article-item {
    background: #E5E5E5;
    padding: 15px 20px;
    margin: 0 0 80px;
  }

  .article-header {
    display: flex;
    align-items: center;
    padding: 10px 15px;
    margin: -15px -20px 15px;
    background: #C8C8C8;
  }

  .flex-wrapper {
    display: flex;
  }

  .width-50 {
    width: 50%;
    padding-right: 20px;
  }

  .width-50:nth-last-child(1) {
    padding-right: 0;
    padding-left: 20px;
  }

  .article-section-title {
    display: block;
    margin: 0 0 8px;
    font-size: 22px!important;
    font-weight: 600;
  }

  .article-hd-textlg {
    font-size: 22px;
  }

  .article-hd-textsm {
    font-size: 18px;
  }

  .article-header-link {
    font-size: 22px;
    padding: 0 32px 0 8px;
  }

  .article-header-icons {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    flex: 1;
  }

  .article-header-icons >>> i {
    margin-right: 5px;
    font-size: 18px;
    color: #979797;
    cursor: pointer;
    transition: all ease-in-out .25s;
  }

  .article-header-icons >>> i:nth-last-child(1) {
    margin: 0;
  }

  .article-header-icons >>> i:hover {
    color: #000;
  } 

  .header-func-icons {
    padding-left: 20px;
  }

  .notes-toc-item >>> p {
    margin: 0;
  }

  .notes-toc-item >>> a {
    text-decoration: none;
    transition: all ease-in-out .25s;
  }

  .notes-toc-item >>> a:hover {
    color: #7eb2e5; 
  }

  .notes-settings {
    display: flex;
    width: 100%;
    padding: 0 0 12px;
  }

  .notes-settings-btn:nth-last-child(1) {
    margin-left: 20px;
  }

  .notes-content {
    overflow: hidden;
    height: 0;
  }

  .notes-content.open {
    height: 100%;
  }

  @media screend and (max-width: 768px) {
    .article-item {
      margin: 0 0 30px;
    }
    .article-hd-textlg {
      font-size: 20px;
    }
    .article-hd-textsm {
      font-size: 16px;
    }
    .article-header-link {
      font-size: 20px;
      padding: 0 32px 0 8px;
    }
  }
</style>
