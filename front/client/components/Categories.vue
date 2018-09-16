<template>
  <v-container grid-list-md>
    <v-layout
      row
      wrap
      justify-space-between
    >
      <v-flex
        class="mr-3 mt-3"
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
          <h4 class="mb-2 display-1 font-weight-black"> 
            {{ groupName }}
          </h4>

          <a-link
            class="category-title"
            openInNewTab
            v-for="category in groupCategories[CategoryStatus.finished]"
            :key="category.uid"
            :url="`https://guide.aelve.com/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 subheading font-weight-bold"
            >
              {{ category.title }} 
            </h6>
          </a-link>

          <h6
            class="ml-2 body-2 font-weight-bold"
            v-if="groupCategories[CategoryStatus.inProgress]"
          >
            In progress
          </h6>
          <a-link
            class="category-title ml-3"
            openInNewTab
            v-for="category in groupCategories[CategoryStatus.inProgress]"
            :key="category.uid"
            :url="`https://guide.aelve.com/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 subheading font-weight-bold"
            >
              {{ category.title }} 
            </h6>
          </a-link>

          <h6
            class="ml-2 body-2 font-weight-bold"
            v-if="groupCategories[CategoryStatus.toBeWritten]"
          >
            To be written
          </h6>
          <a-link
            class="category-title ml-3"
            openInNewTab
            v-for="category in groupCategories[CategoryStatus.toBeWritten]"
            :key="category.uid"
            :url="`https://guide.aelve.com/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 subheading font-weight-bold"
            >
              {{ category.title }} 
            </h6>
          </a-link>
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
import { ICategory, CategoryStatus } from 'client/service/Category'


@Component
export default class Categories extends Vue {
  CategoryStatus = CategoryStatus
  // TODO add type for store
  async asyncData({ store }) {
    return store.dispatch('category/loadCategoryList')
  }
  // TODO create state getter and replace to it
  get categories() {
    return this.$store.state.category.categoryList
  }
  get groups() {
    const groupedByGroupName: object = _groupBy(this.categories, 'group')
    Object.entries(groupedByGroupName).forEach(([key, value]: [string, ICategory[]]) => {
      groupedByGroupName[key] = this.groupByCategoriesByStatus(value)
    })
    return groupedByGroupName
  }
  groupByCategoriesByStatus(categories: ICategory[]): object {
    return _groupBy(categories, 'status')
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
.category-title {
  display: block;
  line-height: 1.2;
}
.category-title:not(:last-child) {
  margin-bottom: 5px;
}
</style>