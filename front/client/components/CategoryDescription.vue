<template>
  <div class="category-description">
    <div v-if="!editDescriptionShown">
      <p v-if="!categoryDescription">This category has no description yet, you can contribute to the category by adding description</p>
      <div class="description-content" v-else v-html="categoryDescription" />
    </div>

    <markdown-editor
      v-if="editDescriptionShown"
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
      <v-icon size="14" class="mr-1" left>{{descriptionBtnIcon}}</v-icon>
      {{descriptionBtnText}}
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
  descriptionButtonIcon: string = ''
  descriptionButtonText: string = ''

  get categoryDescription () {
    return _get(this, '$store.state.category.category.description.html')
  }

  get categoryDscMarkdown () {
    return _get(this, '$store.state.category.category.description.text')
  }

  get categoryId () {
    return this.$store.state.category.category.id
  }

  get descriptionBtnIcon () {
    const description = _get(this, '$store.state.category.category.description.html')
    return description ? this.descriptionButtonIcon = '$vuetify.icons.pen' : this.descriptionButtonIcon = '$vuetify.icons.plus'
  }

  get descriptionBtnText () {
    const description = _get(this, '$store.state.category.category.description.html')
    return description ? this.descriptionButtonText = 'edit description' : this.descriptionButtonText = 'add description'
  }
  

  toggleEditDescription () {
    this.editDescriptionShown = !this.editDescriptionShown
  }

  async updateCategoryDescription (original, modified) {
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
      } else {
        throw err
      }
    }

    this.toggleEditDescription();
  }

  saveDescription(newValue: string) {
    this.updateCategoryDescription(this.originalDescription, newValue)
  }

  saveConflictDescription (modified) {
    this.updateCategoryDescription(this.serverModified, modified)
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

.description-content {
  white-space: pre-wrap;
}
</style>

