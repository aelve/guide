<template>
  <v-container>
    <div class="article-wrapper">
      <div class="article-top">
        <i class="fas fa-rss"/>
        <div
          class="article-top-data"
          v-for="(value, key) in getCategory"
          :key="key"
        >
          <a
            class="article-top-link"
            href="https://guide.aelve.com/haskell/lenses-sth6l9jl"
          >
            {{value.title}}
          </a>
          <p class="article-top-group"> {{value.group}} </p>
        </div>
        <v-btn
          flat
          class="ml-2 pl-0"
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
      <div
        v-for="(value, index) in getCategoryItems"
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
      <v-btn
        flat
        class="ml-2 pl-0"
        color="grey"
        @click="openAddItemDialog"
      >
        <v-icon class="mr-1" left>add</v-icon>
        TATUKA
      </v-btn>
      <add-item-dialog v-model="isDialogOpen"/>
    </div>
  </v-container>
</template>

<script lang="ts">
import _get from 'lodash/get'
import Vue from 'vue'
import Component from 'vue-class-component'
import ArticleContent from 'client/components/ArticleContent.vue'
import AddItemDialog from 'client/components/AddItemDialog.vue'

@Component({
  components: {
    ArticleContent,
    AddItemDialog
  }
})
export default class ArticleItem extends Vue {
  isDialogOpen: boolean = false

  async asyncData() {
    await this.$store.dispatch('categoryItem/loadCategoryItem')
  }

  get categoryDescription() {
    return _get(this, '$store.state.categoryItem.categoryItemList.description.html')
  }

  get getCategory() {
    return this.$store.state.categoryItem
  }

  get getCategoryItems() {
    return this.$store.state.categoryItem.categoryItemList.items
  }

  openAddItemDialog() {
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
