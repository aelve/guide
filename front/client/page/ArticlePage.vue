<template>
  <article-item 
    :category="category"
  />
</template>

<script lang="ts">
import Vue from 'vue'
import ArticleItem from 'client/components/Article.vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator';

@Component({
  name: 'article-page',
  components: {
    ArticleItem
  }
})

export default class ArticlePage extends Vue {
  @Prop(String) category!: string
}
</script>
