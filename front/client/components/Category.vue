<template>
  <v-container>
    <div class="category-wrapper">
      <template v-if="category">
        <category-info
          :category="category" 
          :categoryId="categoryId"
          :categoryTitle="category.title"
          :categoryGroup="category.group"
          :categoryUrl="categoryUrl"
          @openDialog="openAddItemDialog"
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
        />
      </template>
      <v-btn
        flat
        class="ml-2"
        color="grey"
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
import _get from 'lodash/get'
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import CategoryItem from 'client/components/CategoryItem.vue'
import CategoryDescription from 'client/components/CategoryDescription.vue'
import category from 'client/store/modules/category'
import CategoryInfo from 'client/components/CategoryInfo.vue'
import AddItemDialog from 'client/components/AddItemDialog.vue'

@Component({
  components: {
    CategoryItem,
    CategoryDescription,
    CategoryInfo,
    AddItemDialog,
  }
})
export default class Category extends Vue {
  @Prop(String) categoryId!: string

  isAddItemDialogOpen: boolean = false

  get category () {
    return this.$store.state.category.category
  }

  get categoryUrl () {
    return this.category && `${_toKebabCase(this.category.title)}-${this.category.id}`
  }

  // TODO handle case when category was deleted. Go back in that case
  async serverPrefetch () {
    if (!this.categoryId) {
      return
    }
    await this.$store.dispatch('category/loadCategory', this.categoryId)
  }

  beforeDestroy () {
    this.$store.commit('category/setCategory', null)
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

@media screend and (max-width: 768px) {
  .category-item {
    margin: 0 0 30px;
  }
}
</style>
