<template>
  <v-container>
    <div class="category-wrapper">
      <div class="category-top">
        <i class="fas fa-rss"/>
        <div
          v-if="category"
          class="category-top-data"
        >
          <router-link
            class="category-top-link"
            :to="categoryUrl"
          >
            {{category.title}}
          </router-link>
          <p class="category-top-group"> {{category.group}} </p>
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
      </div>
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
        Add new item
      </v-btn>
      <add-item-dialog 
        v-model="isDialogOpen"
        :categoryId="categoryId"
      />
    </div>
  </v-container>
</template>

<script lang="ts">
import _toKebabCase from 'lodash/kebabCase'
import _get from 'lodash/get'
import { Vue, Component, Prop } from 'vue-property-decorator'
import CategoryItem from 'client/components/CategoryItem.vue'
import AddItemDialog from 'client/components/AddItemDialog.vue'
import CategoryDescription from 'client/components/CategoryDescription.vue'
import category from 'client/store/modules/category'

@Component({
  components: {
    CategoryItem,
    AddItemDialog,
    CategoryDescription
  }
})
export default class Category extends Vue {
  @Prop(String) categoryId!: string

  isDialogOpen: boolean = false

  async asyncData () {
    if (!this.categoryId) {
      return
    }
    await this.$store.dispatch('category/loadCategory', this.categoryId)
  }

  beforeDestroy () {
    this.$store.commit('category/setCategory', {})
  }

  get category () {
    return this.$store.state.category.category
  }

  get categoryUrl () {
    return this.category && `${_toKebabCase(this.category.title)}-${this.category.id}`
  }

  openAddItemDialog () {
    this.isDialogOpen = true
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
  margin-right: 30px;
}

.category-top-group {
  font-size: 24px;
}

.category-top-link:hover {
  color: #000;
  color: #979797;
}

.category-wrapper {
  max-width: 800px;
  margin: 0 auto;
}

.category-description {
  margin: 0 0 40px;
}

.category-description >>> p {
  font-size: 16px;
}

.category-description >>> h1 {
  margin: 20px 0 5px;
}

.category-description >>> div:first-child {
  white-space: pre-wrap;
}

@media screend and (max-width: 768px) {
  .category-item {
    margin: 0 0 30px;
  }
}
</style>
