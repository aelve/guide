<template>
  <v-container grid-list-md>
    <v-layout
      row
      wrap
      justify-space-between
    >
      <v-flex
        class="mr-3 mt-4"
        column
        xs12
        sm5
        md3
        lg3
        xl1
        v-for="(groupCategories, groupName, index) in groups" 
        :key="index"
      >
        <div class="category-group">
          <h4 class="display-1 font-weight-black"> {{ groupName }} </h4>
          <a
            class="category-title"
            target="_blank" 
            rel="noopener noreferrer" 
            v-for="(category, index) in groupCategories"
            :key="index"
            :href="`https://guide.aelve.com/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 subheading font-weight-bold"
              :key="index"
            >
              {{ category.title }} 
            </h6>
          </a>
        </div>
      </v-flex>
    </v-layout>
  </v-container>
</template>

<script lang="ts">
import _groupBy from 'lodash/groupBy'
import _toKebabCase from 'lodash/kebabCase'
import Vue from 'vue'
import Component from 'vue-class-component'
import { ICategory } from 'client/service/Category'

@Component
export default class Categories extends Vue {
  // TODO add type for store
  async asyncData({ store }) {
    return store.dispatch('category/loadCategoryList')
  }
  // TODO create state getter and replace to it
  get categories() {
    return this.$store.state.category.categoryList
  }
  get groups() {
    return _groupBy(this.categories, 'group')
  }
  getCategoryUrl(category: ICategory): string {
    return `${_toKebabCase(category.title)}-${category.uid}`
  }
}
</script>

<style scoped>
.category-group {
  text-align: left;
}
.category-title:not(:last-child) {
  margin-bottom: 5px;
}
.category-title {
  display: block;
  text-decoration-line: none;
}
.category-title:hover {
  text-decoration-line: underline;
}
</style>