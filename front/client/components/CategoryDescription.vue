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
      @save="updateDescription({original: originalDescription, modified: $event})"
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
  </div>
</template>

<script lang="ts">
import { Vue, Component, Prop } from 'vue-property-decorator'
import _get from 'lodash/get'
import MarkdownEditor from 'client/components/MarkdownEditor.vue'
import conflictDialogMixin from 'client/mixins/conflictDialogMixin'
import CatchConflictDecorator from 'client/helpers/CatchConflictDecorator'

@Component({
  components: {
    MarkdownEditor
  },
  mixins: [conflictDialogMixin]
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

  @CatchConflictDecorator
  async updateDescription ({ original, modified }) {
    await this.$store.dispatch('categoryItem/updateCategoryDescription', {
      id: this.categoryId,
      original,
      modified
    })
    this.originalDescription = modified
    this.toggleEditDescription()
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

