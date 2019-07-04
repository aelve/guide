<template>
  <div class="category-top">
    <div
      class="category-top-data"
    >
      <a-link
        openInNewTab
        :url="`https://guide.aelve.com/haskell/feed/category/${categoryId}`"
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
        title="Edit item info"
        icon="cog"
        iconSize="18"
        @click="openCategoryInfoDialog"
      />
    </div>
    <v-btn
      class="ma-0 px-1"
      flat
      title="Add new item"
      color="grey"
      @click="openDialog"
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
  </div>  
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import CategoryInfoEdit from 'client/components/CategoryInfoEdit.vue'
import ALink from 'client/components/ALink.vue'
import Confirm from 'client/helpers/ConfirmDecorator'
import CategoryItemBtn from 'client/components/CategoryItemBtn.vue'

@Component({
  components: {
    CategoryInfoEdit,
    ALink,
    CategoryItemBtn,
  }
})
export default class CategoryInfo extends Vue {
  @Prop(Object) category!: object
  @Prop(String) categoryId!: string
  @Prop(String) categoryTitle!: string
  @Prop(String) categoryGroup!: string
  @Prop(String) categoryUrl!: string

  isCategoryInfoEdit: boolean = false
  isAddItemDialogOpen: boolean = false

  openCategoryInfoDialog () {
    this.isCategoryInfoEdit = true
  }

  openDialog () {
    this.$emit('openAddItemDialog');
  }

  @Confirm({ text: 'delete this category' })
  async deleteCategory () {
    if (!this.category) {
      return
    }
    await this.$store.dispatch('category/deleteCategory', this.categoryId)
    this.$router.back()
  }
}
</script>

<style scoped>
.category-top {
  display: flex;
  align-items: center;
  margin: 0 0 5px;
}

.category-top-data {
  display: flex;
  align-items: center;
  flex: 1;
}

.category-top >>> i {
  margin-right: 15px;
  font-size: 18px;
  color: #979797;
  cursor: pointer;
  transition: all ease-in-out 0.25s;
}

.category-top >>> i:hover {
  color: #000;
}

.category-top-link {
  font-size: 24px;
  font-weight: 600;
  text-decoration: none;
  color: #979797;
  cursor: pointer;
  transition: all ease-in-out 0.25s;
}

.category-top-link:hover {
  color: #000;
}

.category-top-group {
  font-size: 24px;
}
</style>

