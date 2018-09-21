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
        <!-- TODO remove when links refactored -->
        <!-- <router-link to="/haskell">
          <button class="test-btn">Test Article</button>
        </router-link> -->
        
        <div class="category-group">
          <h4 class="mb-2 display-1 font-weight-black">
            {{ groupName }}
          </h4>
          <!-- TODO remove duplicates of same a-links -->
          <a-link
            class="category-title"
            openInNewTab
            v-for="category in groupCategories[CategoryStatus.finished]"
            :key="category.uid"
            :url="`http://aelve.com:4801/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 subheading font-weight-bold"
            >
              {{ category.title }}
            </h6>
          </a-link>

          <h6
            class="ml-2 mb-1 body-2 font-weight-bold"
            v-if="groupCategories[CategoryStatus.inProgress]"
          >
            In progress
          </h6>
          <a-link
            class="category-title ml-3"
            openInNewTab
            v-for="category in groupCategories[CategoryStatus.inProgress]"
            :key="category.uid"
            :url="`http://aelve.com:4801/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 body-1 font-weight-bold"
            >
              {{ category.title }}
            </h6>
          </a-link>

          <h6
            class="ml-2 mb-1 body-2 font-weight-bold"
            v-if="groupCategories[CategoryStatus.toBeWritten]"
          >
            To be written
          </h6>
          <a-link
            class="category-title ml-3"
            openInNewTab
            v-for="category in groupCategories[CategoryStatus.toBeWritten]"
            :key="category.uid"
            :url="`http://aelve.com:4801/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 body-1 font-weight-bold"
            >
              {{ category.title }}
            </h6>
          </a-link>

          <add-category-dialog
            :groupName="groupName"
          >
            <v-btn class="ml-2 pl-0" flat color="grey">
              <v-icon class="mr-1" left>add</v-icon>
              Add new category
            </v-btn>
          </add-category-dialog>
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
import AddCategoryDialog from 'client/components/AddCategoryDialog.vue'

@Component({
  components: {
    AddCategoryDialog
  }
})
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
.test-btn {
  background: #000;
  color: #fff;
  padding: 8px 14px 7px;
  border-radius: 25px;
  text-transform: uppercase;
  margin: 0 0 20px;
}
</style>