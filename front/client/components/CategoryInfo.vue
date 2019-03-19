<template>
  <div class="category-top">
    <div
      v-if="category"
      class="category-top-data"
    >
      <a-link
        openInNewTab
        :url="`/haskell/feed/category/${categoryId}`"
      >
        <v-icon size="14" class="mr-3" left>$vuetify.icons.rss</v-icon>
      </a-link>
      <p class="category-top-group mr-3"> {{category.group}} </p>
      <v-icon size="8" class="mr-3" left>$vuetify.icons.circle</v-icon>
      <router-link
        class="category-top-link mr-3"
        :to="categoryUrl"
      >
        {{category.title}}
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
  </div>  
</template>

<script lang="ts">
import Vue from 'vue'

export default class CategoryInfo extends Vue {

}
</script>
