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
          placeholder="Write new description here"
          v-model="textareaHasDescription"
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
            @click="toggleEditDescription(); addCategoryDescription(originalDescription, textareaHasDescription);"
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
      <!-- END When no category description show stub -->
      <div
        v-if="categoryDescription"
        class="article-description"
      >
        <div v-if="!editDescriptionShown" v-html="categoryDescription" />
        <v-textarea
          v-if="editDescriptionShown"
          solo
          name="input-7-4"
          label="Solo textarea"
          :value="textareaHasDescription"
          v-model="textareaHasDescription"
          auto-grow
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
          <v-icon class="mr-1" left>edit</v-icon>
          Edit description
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
            @click="toggleEditDescription(); addCategoryDescription(originalDescription, textareaHasDescription);"
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
      <template v-if="category">
        <div
          v-for="(value, index) in category.items"
          :key="index"
        > 
          <category-item
            :kind="value.name"
            :group="value.group"
            :itemDescription="value.summary.html"
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
      <conflict-dialog
        v-model="isDescriptionConflict"
        :serverModified="serverModified"
        :modified="modified"
        :merged="merged"
        v-on:saveDescription="saveConflictDescription"
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
import ConflictDialog from 'client/components/ConflictDialog.vue'
import category from 'client/store/modules/category'

@Component({
  name: 'article-component',
  components: {
    CategoryItem,
    AddItemDialog,
    ConflictDialog
  }
})
export default class Category extends Vue {
  @Prop(String) categoryId!: string
  @Prop(String) categoryDsc!: string

  isDialogOpen: boolean = false
  editDescriptionShown: boolean = false
  catUid: string = ''
  emptyDescription: string = ''
  isDescriptionConflict: boolean = false
  serverModified: string = ''
  modified: string = ''
  merged: string = ''
  textareaHasDescription: string = ''
  originalDescription: string = this.categoryDscMarkdown
  modifiedDescription: string = !this.categoryDscMarkdown ? '' : this.categoryDscMarkdown

  async asyncData () {
    if (!this.categoryId) {
      return
    }
    await this.$store.dispatch('category/loadCategory', this.categoryId)
    this.originalDescription = this.categoryDscMarkdown
  }

  get categoryDescription () {
    return _get(this, '$store.state.category.category.description.html')
  }

  get categoryDscMarkdown () {
    this.textareaHasDescription = _get(this, '$store.state.category.category.description.text')
    return _get(this, '$store.state.category.category.description.text')
  }

  get category () {
    return this.$store.state.category.category
  }

  get categoryUrl () {
    return this.category && `${_toKebabCase(this.category.title)}-${this.category.uid}`
  }

  get categoryUid () {
    return this.$store.state.category.category.uid
  }

  openAddItemDialog () {
    this.isDialogOpen = true
  }

  toggleEditDescription () {
    if (this.editDescriptionShown === false) {
      this.editDescriptionShown = true
      return
    }

    this.editDescriptionShown = false
  }

  async addCategoryDescription (original: string, modified: string) {
    try {
      await this.$store.dispatch('categoryItem/addCategoryDescription', {
        uid: this.categoryUid,
        original: original,
        modified: modified
      })
      this.$store.dispatch('category/reloadCategory', null, { root: true })
    } catch (err) {
      if (err.response.status === 409) {
        console.table(err)
        this.serverModified = err.response.data.server_modified
        this.modified = err.response.data.modified
        this.merged = err.response.data.merged
        this.isDescriptionConflict = true
      }
      throw err
    }
  }

  saveConflictDescription (data: any) {
    let { original, modified } = data

    this.addCategoryDescription(original, modified)
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
