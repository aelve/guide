<template>
  <v-container grid-list-md>
    <v-layout
      row
      wrap
      justify-space-between
    >
      <v-flex
        class="mr-3"
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
            v-for="(category, index) in groupCategories"
            :key="index"
            href="#"
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
import Vue from 'vue'
import Component from 'vue-class-component'

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
}
</script>

<style scoped>
.category-group {
  text-align: left;
}
.category-title {
  text-decoration-line: none;
}
.category-title:hover {
  text-decoration-line: underline;
}
</style>