<template>
  <v-container>
    <div class="category-wrapper">
      <template v-if="category">
        <CategoryHeader
          :category="category" 
          :categoryId="categoryId"
          :categoryTitle="category.title"
          :categoryGroup="category.group"
          :categoryUrl="categoryUrl"
          @openAddItemDialog="openAddItemDialog"
        />
      </template>

      <category-description />

      <template v-if="category">
        <category-item
          v-for="value in category.items"
          :key="value.id"
          :itemUid="value.id"
          :link="value.link"
          :name="value.name"
          :group="value.group"
          :summary="value.summary"
          :pros="value.pros"
          :cons="value.cons"
          :ecosystem="value.ecosystem"
          :hackage="value.hackage"
          :toc="value.toc"
          :notes="value.notes"
          :kind="value.kind"
          :sections="category.sections"
        />
      </template>
      <v-btn
        v-if="category.items && category.items.length"
        text
        class="ml-2"
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

@media screen and (max-width: 768px) {
  .category-item {
    margin: 0 0 30px;
  }
}
</style>
