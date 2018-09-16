<template>
  <v-container>

  </v-container>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component';
import { IArticle } from 'client/service/Article'

@Component
export default class Article extends Vue {

  async asyncData({ store }) {
    return store.dispatch('category/loadArticleList')
  }

  get articles() {
    return this.$store.state.article.articlesList
  }
}
</script>
