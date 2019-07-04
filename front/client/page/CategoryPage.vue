<template>
  <category
    :categoryId="categoryId"
  />
</template>

<script lang="ts">
import Vue from 'vue'
import Category from 'client/components/Category.vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'

@Component({
  components: {
    Category
  }
})

export default class CategoryPage extends Vue {
  @Prop(String) categoryId!: string

  get category () {
    return this.$store.state.category.category
  }

  // TODO handle case when category was deleted. Go back in that case
  async serverPrefetch () {
    if (!this.categoryId) {
      return
    }
    await this.$store.dispatch('category/loadCategory', this.categoryId)
  }

  beforeMount () {
    this.$watch('category', this.setDocumentTitle, { immediate: true })
  }

  beforeDestroy () {
    this.$store.commit('category/setCategory', null)
  }

  setDocumentTitle (category) {
    document.title = category
      ? `${category.title} – Aelve Guide`
      : 'Aelve Guide'
  }
}
</script>
