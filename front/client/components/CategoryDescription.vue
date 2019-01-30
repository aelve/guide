<template>
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
    <conflict-dialog
      v-model="isDescriptionConflict"
      :serverModified="serverModified"
      :modified="modified"
      :merged="merged"
      @saveDescription="saveConflictDescription"
    />
  </div>
</template>

<script lang="ts">
import { Vue, Component, Prop } from 'vue-property-decorator'
import _get from 'lodash/get'
import MarkdownEditor from 'client/components/MarkdownEditor.vue'
import ConflictDialog from 'client/components/ConflictDialog.vue'

@Component({
  components: {
    MarkdownEditor,
    ConflictDialog
  }
})
export default class CategoryDescriptiom extends Vue {
  editDescriptionShown: boolean = false
  isDescriptionConflict: boolean = false
  originalDescription: string = _get(this, '$store.state.category.category.description.text')
  serverModified: string = ''
  modified: string = ''
  merged: string = ''

  get categoryDescription () {
    return _get(this, '$store.state.category.category.description.html')
  }

  get categoryDscMarkdown () {
    return _get(this, '$store.state.category.category.description.text')
  }

  get categoryId () {
    return this.$store.state.category.category.id
  }

  toggleEditDescription () {
    this.editDescriptionShown = !this.editDescriptionShown
  }

  async updateCategoryDescription (original: any, modified: any) {
    try {
      await this.$store.dispatch('categoryItem/updateCategoryDescription', {
        id: this.categoryId,
        original: original,
        modified: modified
      })
      this.originalDescription = modified
    } catch (err) {
      if (err.response.status === 409) {
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
</style>

