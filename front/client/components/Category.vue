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
          class="ml-2 pl-0 add-item-btn"
          flat
          color="grey"
          @click="openAddItemDialog"
        >
          <v-icon class="mr-1" left>add</v-icon>
          Add new item
        </v-btn>
      </div>
      <div
        v-if="categoryDescription"
        class="category-description"
      >
        <div v-html="categoryDescription" />
      </div>
      <template v-if="category">
        <category-item
          v-for="value in category.items"
          :key="value.uid"
          :itemUid="value.uid"
          :link="value.link"
          :name="value.name"
          :group="value.group"
          :itemDescription="value.description.html"
          :pros="value.pros"
          :cons="value.cons"
          :ecosystem="value.ecosystem.html"
          :tocArray="value.toc"
          :notes="value.notes.html"
          :kind="value.kind"
        />
      </template>
      <v-btn
        flat
        class="ml-2 pl-0"
        color="grey"
        @click="openAddItemDialog"
      >
        <v-icon class="mr-1" left>add</v-icon>
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
import category from 'client/store/modules/category'

@Component({
  name: 'category-component',
  components: {
    CategoryItem,
    AddItemDialog
  }
})
export default class categoryItem extends Vue {
  @Prop(String) categoryId!: string

  isDialogOpen: boolean = false

  async asyncData () {
    if (!this.categoryId) {
      return
    }
    await this.$store.dispatch('category/loadCategory', this.categoryId)
  }

  get categoryDescription () {
    return _get(this, '$store.state.category.category.description.html')
  }

  get category () {
    return this.$store.state.category.category
  }

  get categoryUrl () {
    return this.category && `${_toKebabCase(this.category.title)}-${this.category.uid}`
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
  margin: 0 0 45px;
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
  width: 800px;
  margin: 0 auto;
}

.category-description {
  margin: 0 0 60px;
}

.category-description >>> p {
  font-size: 16px;
}

.category-description >>> h1 {
  margin: 20px 0 5px;
}

@media screend and (max-width: 768px) {
  .category-item {
    margin: 0 0 30px;
  }
}
</style>
