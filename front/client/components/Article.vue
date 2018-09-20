<template>
  <v-container>
    <!-- {{ article.notes }} `-->
    <h1>TEST BLYAT</h1>
    <p> {{ getCategory }} </p>
    <p>more more more text</p>
    <p> {{ getCategoryDescription }} </p>
  </v-container>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'


@Component
export default class ArticleItem extends Vue {

  async asyncData({ store }) {
    return store.dispatch('category/loadCategoryItem')
  }

  get getCategory () {
    return typeof this.$store.state.category.categoryList
  }

  get getCategoryDescription() {
    // let rawDescription = this.$store.state.category.categoryList.description.html;

    // let parsedDescription = new DOMParser().parseFromString(rawDescription)

    // return typeof this.$store.state.category.categoryList.description.html
    // return parsedDescription
    return this.$store.state.category.categoryList
  }

  // const computed = {
  //   getCategory: function() {
  //     const categoryData = this.$store.state.category.categoryList.items;

  //     // categoryData.map() => {

  //     // }

  //     return this.$store.state.category.categoryList.items
  //   },

  //   getCategoryDescription: function() {
  //     return this.$store.state.category.categoryList.description.html
  //   }
  // }

  // stripHtml() {
  //   const categoryData: any[] = computed.getCategory();
  // }
  // const parser = new DOMParser()
}
</script>
