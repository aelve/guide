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
          <h1 class="mt-0 mb-2 display-1 font-weight-black">
            {{ groupName }}
          </h1>

          <router-link
            class="category-title ml-2"
            v-for="category in groupCategories[CategoryStatus.finished]"
            :key="category.id"
            :to="`/haskell/${getCategoryUrl(category)}`"
          >
            {{ category.title }}
          </router-link>

          <template v-if="groupCategories[CategoryStatus.inProgress]">
            <h3 class="status-title">
              In progress
            </h3>
            <router-link
              class="category-title ml-3"
              v-for="category in groupCategories[CategoryStatus.inProgress]"
              :key="category.id"
              :to="`/haskell/${getCategoryUrl(category)}`"
            >
              {{ category.title }}
            </router-link>
          </template>

          <template v-if="groupCategories[CategoryStatus.toBeWritten]">
            <h3 class="status-title">
              To be written
            </h3>

            <router-link
              class="category-title ml-3"
              v-for="category in groupCategories[CategoryStatus.toBeWritten]"
              :key="category.id"
              :to="`/haskell/${getCategoryUrl(category)}`"
            >
              {{ category.title }}
            </router-link>
          </template>

          <v-btn
            flat
            class="ma-0 px-1"
            color="grey darken-2"
            title="Add new category"
            @click="openAddCategoryDialog(groupName)"
          >
            <v-icon size="14" class="mr-1" left>$vuetify.icons.plus</v-icon>
            Add new category
          </v-btn>
        </div>
      </v-flex>
      <add-category-dialog
        v-model="isAddGroupDialogOpen"
        :groupName="addCategoryGroupName"
      />
    </v-layout>
  </v-container>
</template>

<script lang="ts">
import _groupBy from 'lodash/groupBy'
import _toKebabCase from 'lodash/kebabCase'
import _sortBy from 'lodash/sortBy'
import _fromPairs from 'lodash/fromPairs'
import Vue from 'vue'
import Component from 'vue-class-component'
import { ICategoryInfo, CategoryStatus } from 'client/service/Category'
import AddCategoryDialog from 'client/components/AddCategoryDialog.vue'

@Component({
  components: {
    AddCategoryDialog
  }
})
export default class Categories extends Vue {
  CategoryStatus = CategoryStatus
  addCategoryGroupName: string = ''
  isAddGroupDialogOpen: boolean = false

  async serverPrefetch () {
    return this.$store.dispatch('category/loadCategoryList')
  }

  get categories () {
    return this.$store.state.category.categoryList
  }

  get groups () {
    const groupedByGroupName = _groupBy(this.categories, 'group')
    const groupedEntries = Object.entries(groupedByGroupName)
    const groupedAlsoByStatus = groupedEntries.map(
      ([groupName, groupCategories]: [string, ICategoryInfo[]]) => {
        const groupedCategoriesByStatus = _groupBy(groupCategories, 'status')
        return [groupName, groupedCategoriesByStatus]
      })
    // Key 0 is groupName, we sort by groupNames
    const entriesGroupNameIndex = 0
    const sortedAlphabetically = _sortBy(groupedAlsoByStatus, entriesGroupNameIndex)
    return _fromPairs(sortedAlphabetically)
  }

  openAddCategoryDialog (groupName: string) {
    this.addCategoryGroupName = groupName
    this.isAddGroupDialogOpen = true
  }

  getCategoryUrl (category: ICategoryInfo): string {
    return `${_toKebabCase(category.title)}-${category.id}`
  }
}
</script>

<style scoped>
.category-group {
  text-align: left;
}
.category-title {
  display: block;
  font-size: 0.8rem;
  line-height: 1.2;
  font-weight: 600;
}
.status-title {
  font-size: 0.75rem;
  margin: 0 0 4px 8px;

}
.category-title:not(:last-child) {
  margin-bottom: 5px;
}
</style>