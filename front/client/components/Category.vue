<template>
  <v-container>
    <div
      class="category-wrapper"
      v-if="category"
    >
      <CategoryHeader
        :category="category" 
        @openAddItemDialog="openAddItemDialog"
      />

      <CategoryDescription :category="category" />

      <CategoryItem
        v-for="value in category.items"
        :key="value.id"
        :item="value"
        :sections="category.sections"
      />

      <v-btn
        v-if="category.items && category.items.length"
        text
        class="ml-2 category__add-item-lower-btn"
        color="grey darken-2"
        aria-label="Add new item"
        @click="openAddItemDialog"
      >
        <v-icon size="14" class="mr-1" left>$vuetify.icons.plus</v-icon>
        Add new item
      </v-btn>

      <add-item-dialog
        v-model="isAddItemDialogOpen"
        :categoryId="categoryId"
      />
    </div>
  </v-container>
</template>

<script lang="ts">
import _toKebabCase from 'lodash/kebabCase'
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import CategoryItem from 'client/components/CategoryItem.vue'
import CategoryDescription from 'client/components/CategoryDescription.vue'
import CategoryHeader from 'client/components/CategoryHeader.vue'
import AddItemDialog from 'client/components/AddItemDialog.vue'

@Component({
  components: {
    CategoryItem,
    CategoryDescription,
    CategoryHeader,
    AddItemDialog
  }
})
export default class Category extends Vue {
  isAddItemDialogOpen: boolean = false

  get category () {
    return this.$store.state.category.category
  }

  get categoryId () {
    return this.$store.state.category.category.id
  }

  get categoryUrl () {
    return this.category && `${_toKebabCase(this.category.title)}-${this.category.id}`
  }

  openAddItemDialog () {
    this.isAddItemDialogOpen = true
  }
}
</script>

<style scoped>
.category-wrapper {
  max-width: 800px;
  margin: 0 auto;
}

.category__add-item-lower-btn {
  background: #e6e9eaa8;
}

@media screen and (max-width: 768px) {
  .category-item {
    margin: 0 0 30px;
  }
}
</style>
