<template>
  <category />
</template>

<script lang="ts">
import Vue from 'vue'
import Category from 'client/components/Category.vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'

@Component({
  components: {
    Category
  }
})

export default class CategoryPage extends Vue {
  @Prop(String) categoryId!: string

  async serverPrefetch () {
    if (!this.categoryId) {
      return
    }
    await this.$store.dispatch('category/loadCategory', this.categoryId)
  }

  get category () {
    return this.$store.state.category.category
  }

  beforeDestroy () {
    this.$store.commit('category/setCategory', null)
  }
}
</script>
