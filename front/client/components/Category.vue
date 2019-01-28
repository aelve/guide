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
      <div class="category-description">
        <div v-if="!editDescriptionShown">
          <p v-if="!categoryDescription">This category has no description yet, you can contribute to the category by adding description</p>
          <div v-else v-html="categoryDescription" />
        </div>

        <slot v-if="!editDescriptionShown"/>

        <markdown-editor
          v-else
          class="mb-2"
          toolbar
          :value="categoryDscMarkdown"
          @cancel="toggleEditDescription"
          @save="saveDescription"
        />

        <v-btn
          v-if="!editDescriptionShown"
          depressed
          small
          light
          color="lightgrey"
          @click="toggleEditDescription"
        >
          <v-icon v-if="!categoryDescription" size="14" class="mr-1" left>$vuetify.icons.plus</v-icon>
          <v-icon v-else size="14" class="mr-1" left>$vuetify.icons.pen</v-icon>
          <p v-if="!categoryDescription">add description</p>
          <p v-else>edit description</p>
        </v-btn>
      </div>
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
      <conflict-dialog
        v-model="isDescriptionConflict"
        :serverModified="serverModified"
        :modified="modified"
        :merged="merged"
        @saveDescription="saveConflictDescription"
      />
    </div>
  </v-container>
</template>

<script lang="ts">
import _toKebabCase from 'lodash/kebabCase'
import _get from 'lodash/get'
import { Vue, Component, Prop } from 'vue-property-decorator'
import MarkdownEditor from 'client/components/MarkdownEditor.vue'
import CategoryItem from 'client/components/CategoryItem.vue'
import AddItemDialog from 'client/components/AddItemDialog.vue'
import ConflictDialog from 'client/components/ConflictDialog.vue'
import category from 'client/store/modules/category'

@Component({
  name: 'article-component',
  components: {
    CategoryItem,
    AddItemDialog,
    ConflictDialog,
    MarkdownEditor
  }
})
export default class Category extends Vue {
  @Prop(String) categoryId!: string
  @Prop(String) categoryDsc!: string

  isDialogOpen: boolean = false
  editDescriptionShown: boolean = false
  isDescriptionConflict: boolean = false
  serverModified: string = ''
  modified: string = ''
  merged: string = ''
  originalDescription: string = ''

  async asyncData () {
    if (!this.categoryId) {
      return
    }
    this.originalDescription = _get(this, '$store.state.category.category.description.text')
    await this.$store.dispatch('category/loadCategory', this.categoryId)
  }

  beforeDestroy () {
    this.$store.commit('category/setCategory', {})
  }

  get categoryDescription () {
    return _get(this, '$store.state.category.category.description.html')
  }

  get categoryDscMarkdown () {
    return _get(this, '$store.state.category.category.description.text')
  }

  get category () {
    return this.$store.state.category.category
  }

  get categoryUrl () {
    return this.category && `${_toKebabCase(this.category.title)}-${this.category.id}`
  }

  get categoryUid () {
    return this.$store.state.category.category.id
  }

  openAddItemDialog () {
    this.isDialogOpen = true
  }

  toggleEditDescription () {
    this.editDescriptionShown = !this.editDescriptionShown
  }

  async updateCategoryDescription (original: any, modified: any) {
    try {
      await this.$store.dispatch('categoryItem/updateCategoryDescription', {
        id: this.categoryUid,
        original: original,
        modified: modified
      })
      this.originalDescription = modified
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

    this.toggleEditDescription();
  }

  saveDescription(newValue: string) {
    this.updateCategoryDescription(this.originalDescription, newValue)
  }

  saveConflictDescription (data: any) {
    let { original, modified } = data

    this.updateCategoryDescription(original, modified)
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
