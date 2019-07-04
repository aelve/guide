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
          <h4 class="mb-2 display-1 font-weight-black category-group-name">
            {{ groupName }}
          </h4>

          <router-link
            class="category-title"
            v-for="category in groupCategories[CategoryStatus.finished]"
            :key="category.id"
            :to="`/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 subheading font-weight-bold"
            >
              {{ category.title }}
            </h6>
          </router-link>

          <h6
            class="ml-2 mb-1 body-2 font-weight-bold"
            v-if="groupCategories[CategoryStatus.inProgress]"
          >
            In progress
          </h6>
          <router-link
            class="category-title ml-3"
            v-for="category in groupCategories[CategoryStatus.inProgress]"
            :key="category.id"
            :to="`/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 body-1 font-weight-bold"
            >
              {{ category.title }}
            </h6>
          </router-link>

          <h6
            class="ml-2 mb-1 body-2 font-weight-bold"
            v-if="groupCategories[CategoryStatus.toBeWritten]"
          >
            To be written
          </h6>
          <router-link
            class="category-title ml-3"
            v-for="category in groupCategories[CategoryStatus.toBeWritten]"
            :key="category.id"
            :to="`/haskell/${getCategoryUrl(category)}`"
          >
            <h6
              class="ml-2 body-1 font-weight-bold"
            >
              {{ category.title }}
            </h6>
          </router-link>

          <v-btn
            class="ma-0 px-1"
            color="grey"
            title="Add new category"
            flat
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
  line-height: 1.2;
}
.category-title:not(:last-child) {
  margin-bottom: 5px;
}
</style>