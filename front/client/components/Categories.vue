<template>
  <div class="categories">
    <div class="categories-flex-container">

      <AddCategoryBtn
        class="mt-4"
        v-if="!categories || !categories.length"
        @click="openAddCategoryDialog()"
      />

      <div
        class="categories-column"
        v-for="(groupCategories, groupName, index) in groups"
        :key="index"
      >
        <div
          class="category-group"
          data-testid="Categories-CategoryGroup"
        >
          <h2
            class="mt-0 mb-2 group-title"
            data-testid="Categories-CategoryGroup-Title"
          >
            {{ groupName }}
          </h2>

          <CategoriesCategoryLink
            v-for="category in groupCategories[CategoryStatus.finished]"
            :key="category.id"
            :category="category"
            class="ml-2"
          />

          <template v-if="groupCategories[CategoryStatus.inProgress]">
            <h3 class="status-title">
              In progress
            </h3>
            <CategoriesCategoryLink
              v-for="category in groupCategories[CategoryStatus.inProgress]"
              :key="category.id"
              :category="category"
              class="ml-3"
            />
          </template>

          <template v-if="groupCategories[CategoryStatus.toBeWritten]">
            <h3 class="status-title">
              To be written
            </h3>

            <CategoriesCategoryLink
              v-for="category in groupCategories[CategoryStatus.toBeWritten]"
              :key="category.id"
              :category="category"
              class="ml-3"
            />
          </template>

          <AddCategoryBtn @click="openAddCategoryDialog(groupName)"/>
        </div>
      </div>

    </div>

    <add-category-dialog
      v-model="isAddGroupDialogOpen"
      :groupName="addCategoryGroupName"
    />
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import AddCategoryDialog from 'client/components/AddCategoryDialog.vue'
import AddCategoryBtn from 'client/components/AddCategoryBtn.vue'
import CategoriesCategoryLink from 'client/components/CategoriesCategoryLink.vue'
import _groupBy from 'lodash/groupBy'
import _sortBy from 'lodash/sortBy'
import _fromPairs from 'lodash/fromPairs'
import getCategoryUrl from 'client/helpers/getCategoryUrl'
import { ICategoryInfo, CategoryStatus } from 'client/service/Category'

@Component({
  components: {
    AddCategoryDialog,
    AddCategoryBtn,
    CategoriesCategoryLink
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
}
</script>

<style lang="postcss" scoped>
.categories {
  /* Bellow we use negative margins for flex container and it creates unwanted horizontal scroll bar */
  overflow-x: hidden;
}
.categories-flex-container {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  margin: -15px -15px 0 0;
  padding: 0 100px;

  &:after {
    content: "";
    flex: 1;
  }

  @media screen and (max-width: 1150px) {
    padding: 0 60px;
  }

  @media screen and (max-width: 830px) {
    padding: 0 40px;
  }
}
.categories-column {
  margin: 15px 15px 0 0;
  flex-shrink: 0;
  flex-grow: 99999;

  @media screen and (min-width: 1904px) {
    flex-basis: calc(20% - 15px * 5);
    max-width: calc(20% - 15px);
  }
  @media screen and (max-width: 1904px) {
    flex-basis: calc(25% - 15px * 4);
    max-width: calc(25% - 15px);
  }
  @media screen and (max-width: 1150px) {
    flex-basis: calc(33.33333% - 15px * 3);
    max-width: calc(33.33333% - 15px);
  }
  @media screen and (max-width: 830px) {
    flex-basis: calc(50% - 15px * 2);
    max-width: calc(50% - 15px);
  }
  @media screen and (max-width: 480px) {
    flex-basis: calc(100% - 15px * 1);
    max-width: calc(100% - 15px);
  }
}
.category-group {
  display: flex;
  flex-direction: column;
  align-items: baseline;
}
.group-title {
  font-weight: 900;
  font-size: 1.6rem;
  max-width: 90%;
  word-break: break-word;
}
.status-title {
  line-height: 1.2;
}
.status-title {
  font-size: 0.9rem;
  margin: 6px 0 4px 8px;

  + >>> .categories__category-link {
    margin-top: 0;
  }
}
.group-title + * {
  margin-top: 0;
}
</style>