<template>
  <div class="category-top">
    <div
      class="category-top-data"
    >
      <a-link
        openInNewTab
        :url="`/haskell/feed/category/${categoryId}`"
      >
        <v-tooltip bottom>
          <template v-slot:activator="{ on }">
            <v-icon size="14" class="mr-3" left v-on="on">$vuetify.icons.rss</v-icon>
          </template>
          <span>{{`RSS feed of ${categoryTitle} category`}}</span>
        </v-tooltip>
      </a-link>
      <p class="category-top-group mr-3"> {{categoryGroup}} </p>
      <v-icon size="8" class="mr-3" left>$vuetify.icons.circle</v-icon>
      <router-link
        class="category-top-link mr-3"
        :to="categoryUrl"
      >
        {{categoryTitle}}
      </router-link>
      <category-item-btn
        title="edit item info"
        icon="cog"
        iconSize="18"
        @click="openCategoryInfoDialog"
      />
    </div>
    <v-btn
      class="ma-0 px-1"
      flat
      color="grey"
      @click="openAddItemDialog"
    >
      <v-icon size="14" class="mr-1" left>$vuetify.icons.plus</v-icon>
      Add new item
    </v-btn>
    <v-btn
      icon
      flat
      class="ma-0 pa-0"
      color="grey"
      title="Delete category"
      @click="deleteCategory"
    >
      <v-icon size="14">$vuetify.icons.trash-alt</v-icon>
    </v-btn>
     <category-info-edit 
      v-model="isCategoryInfoEdit"
      :categoryId="categoryId"
    />
    <add-item-dialog
      v-model="isAddItemDialogOpen"
      :categoryId="categoryId"
    />
  </div>  
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import CategoryInfoEdit from 'client/components/CategoryInfoEdit.vue'
import AddItemDialog from 'client/components/AddItemDialog.vue'
import ALink from 'client/components/ALink.vue'

@Component({
  components: {
    CategoryInfoEdit,
    AddItemDialog,
    ALink,
  }
})
export default class CategoryInfo extends Vue {
  @Prop(String) categoryId!: string
  @Prop(String) categoryTitle!: string
  @Prop(String) categoryGroup!: string
  @Prop(String) categoryUrl!: string

  isCategoryInfoEdit: boolean = false
  isAddItemDialogOpen: boolean = false

  openCategoryInfoDialog () {
    this.isCategoryInfoEdit = true
  }

  openAddItemDialog () {
    this.isAddItemDialogOpen = true
  }
}
</script>
