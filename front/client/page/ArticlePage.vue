<template>
  <article-item />
</template>

<script lang="ts">
import Vue from 'vue'
import ArticleItem from 'client/components/Article.vue'
import Component from 'vue-class-component'

@Component({
  components: {
    ArticleItem
  }
})

export default class ArticlePage extends Vue {}
</script>
