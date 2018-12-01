<template>
  <v-container>
    <div class="article-wrapper">
      <div class="article-top">
        <i class="fas fa-rss"/>
        <div
          v-if="category"
          class="article-top-data"
        >
          <router-link
            class="article-top-link"
            :to="categoryUrl"
          >
            {{category.title}}
          </router-link>
          <p class="article-top-group"> {{category.group}} </p>
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
        class="article-description"
      >
        <div v-html="categoryDescription" />
      </div>
      <template v-if="category">
        <div
          v-for="(value, index) in category.items"
          :key="index"
        > 
          <article-content
            :kind="value.name"
            :group="value.group"
            :itemDescription="value.description.html"
            :pros="value.pros"
            :cons="value.cons"
            :ecosystem="value.ecosystem.html"
            :tocArray="value.toc"
            :notes="value.notes.html"
            :itemUid="value.uid"
          />
        </div>
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
import ArticleContent from 'client/components/ArticleContent.vue'
import AddItemDialog from 'client/components/AddItemDialog.vue'
import category from 'client/store/modules/category'

@Component({
  name: 'article-component',
  components: {
    ArticleContent,
    AddItemDialog
  }
})
export default class ArticleItem extends Vue {
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
.article-top {
  display: flex;
  align-items: center;
  margin: 0 0 45px;
}

.article-top-data {
  display: flex;
  align-items: center;
  flex: 1;
}

.article-top >>> i {
  margin-right: 15px;
  font-size: 18px;
  color: #979797;
  cursor: pointer;
  transition: all ease-in-out 0.25s;
}

.article-top >>> i:hover {
  color: #000;
}

.article-top-link {
  font-size: 24px;
  font-weight: 600;
  text-decoration: none;
  color: #979797;
  cursor: pointer;
  transition: all ease-in-out 0.25s;
  margin-right: 30px;
}

.article-top-group {
  font-size: 24px;
}

.article-top-link:hover {
  color: #000;
  color: #979797;
}

.article-wrapper {
  width: 800px;
  margin: 0 auto;
}

.article-description {
  margin: 0 0 60px;
}

.article-description >>> p {
  font-size: 16px;
}

.article-description >>> h1 {
  margin: 20px 0 5px;
}

@media screend and (max-width: 768px) {
  .article-item {
    margin: 0 0 30px;
  }
}
</style>
