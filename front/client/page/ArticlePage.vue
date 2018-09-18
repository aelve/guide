<template>
  <articleTest />
</template>

<script lang="ts">
import Vue from 'vue'
import Article from 'client/components/Article.vue'
import Component from 'vue-class-component'

@Component({
  components: {
    Article
  }
})

export default class ArticlePage extends Vue {}
</script>
