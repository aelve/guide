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
          <router-link
            class="article-top-link"
            :to="`${pathname()}`"
          >
            {{value.title}}
          </router-link>
          <p class="article-top-group"> {{value.group}} </p>
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
      <!-- When no category description show stub -->
      <div 
        v-if="categoryDescription == ''"
        class="article-description"
      >
        <p v-if="!editDescriptionShown">This category has no description yet, you can contribute to the category by adding description</p>
        <v-textarea
          v-if="editDescriptionShown"
          solo
          name="input-7-4"
          label="Solo textarea"
          value="Write new description here"
        />
        <v-btn
          v-if="!editDescriptionShown"
          class="pl-0 edit-descr-btn"
          depressed
          small
          light
          color="lightgrey"
          @click="toggleEditDescription"
        >
          <v-icon class="mr-1" left>add</v-icon>
          add description
        </v-btn>
        <v-layout
          v-if="editDescriptionShown" 
          align-center 
          justify-start 
          row
        >
          <v-btn
            class="ml-0"
            depressed
            small
            light
            color="lightgrey"
            @click="toggleEditDescription"
          >
            Save
          </v-btn>
          <v-btn
            class="ml-2"
            depressed
            small
            light
            color="lightgrey"
            @click="toggleEditDescription"
          >
            Cancel
          </v-btn>
        </v-layout>
      </div>

      <div
        v-if="categoryDescription"
        class="article-description"
      >
        <div v-html="categoryDescription" />
        <v-btn
          class="pl-0 edit-descr-btn"
          depressed
          small
          light
          color="lightgrey"
          @click="toggleEditDescription"
        >
          <v-icon class="mr-1" left>edit</v-icon>
          Edit description
        </v-btn>
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
        Add new item
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
import { Prop } from 'vue-property-decorator';
import category from 'client/store/modules/category';

@Component({
  name: 'article-component',
  components: {
    ArticleContent,
    AddItemDialog
  }
})
export default class ArticleItem extends Vue {
  isDialogOpen: boolean = false
  editDescriptionShown: boolean = false
  catUid: string = ''

  async asyncData() {
    const rawUrl = this.$route.params.category
    const categoryUrl = rawUrl.split("-").pop()!.split("#").shift()

    await this.$store.dispatch('categoryItem/loadCategoryItem', categoryUrl)
  }

  get categoryDescription () {
    return _get(this, '$store.state.categoryItem.categoryItemList.description.html')
  }

  get getCategory () {
    return this.$store.state.categoryItem
  }

  get getCategoryItems () {
    return this.$store.state.categoryItem.categoryItemList.items
  }

  get categoryUid () {
    return this.catUid = this.$store.state.categoryItem.uid
  }

  openAddItemDialog () {
    this.isDialogOpen = true
  }

  toggleEditDescription() {
    if (this.editDescriptionShown === false) {
      this.editDescriptionShown = true
      return
    }

    this.editDescriptionShown = false
  }

  pathname() {
    return this.$route.params.category
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

.edit-descr-btn {
  margin: 25px 0 0;
}

@media screend and (max-width: 768px) {
  .article-item {
    margin: 0 0 30px;
  }
}
</style>
