<template>
  <v-container>
    <div v-html="getCategoryDescription"></div>
    <br>
    <br>
    <br>
    <div v-for="(value, index) in getCategoryItems" :key="index"
      class="category-item">
      <div class="category-item">
        <div></div>
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
  .category-item {
    background: #DFDFDF;
    padding: 15px 20px;
    margin: 0 0 30px;
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
</style>

