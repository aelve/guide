<template>
  <v-container>
    <div class="article-content">
      <div v-html="getCategoryDescription"></div>
      <br>
      <br>
      <br>
      <div v-for="(value, index) in getCategoryItems" :key="index"
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
        <p class="article-section-title">Summary</p>
        <div v-html="value.description.html"></div>
        <div class="flex-wrapper">
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
        <p class="article-section-title">Ecosystem</p>
        <div v-html="value.ecosystem.html"></div>
        <p class="article-section-title">Notes</p>
        <div v-html="value.notes.html"></div>
      </div>
    </div>
  </v-container>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'


@Component
export default class ArticleItem extends Vue {

  async asyncData({ store }) {
    return store.dispatch('categoryItem/loadCategoryItem')
  }

  get getCategory () {
    return this.$store.state.categoryItem.categoryItemList
  }

  get getCategoryDescription() {
    const description = this.$store.state.categoryItem.categoryItemList.description.html;
    return description
  }

  get getCategoryItems() {
    const items = this.$store.state.categoryItem.categoryItemList.items

    return items
  }
}
</script>

<style scoped>
  .article-content {
    width: 800px;
    margin: 0 auto;
  }

  .article-item {
    background: #DFDFDF;
    padding: 15px 20px;
    margin: 0 0 30px;
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
    margin: 0;
    font-size: 18px;
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

  /* TODO undestand why it is not working */
  .sourceCode {
    min-width: 100%;
    padding: 8px;
  }

  @media screend and (max-width: 768px) {
    .article-content {
      width: 100%;
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

