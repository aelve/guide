<template>
  <category 
    :categoryId="categoryId"
  />
</template>

<script lang="ts">
import Vue from 'vue'
import Category from 'client/components/Category.vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator';

@Component({
  components: {
    Category
  }
})

export default class ArticlePage extends Vue {
  @Prop(String) categoryId!: string
}
</script>
