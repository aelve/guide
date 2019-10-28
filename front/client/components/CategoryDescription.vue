<template>
  <div class="category-description">
    <div v-if="!isEditDescription">
      <p v-if="!categoryDescription">This category has no description yet, you can contribute to the category by adding description</p>
      <div
        v-else
        v-html="categoryDescription"
        class="category-description__content"
        data-testid="CategoryDescription-Content"
      />
    </div>

    <markdown-editor
      v-else
      toolbar
      data-testid="CategoryDescription-Editor"
      :value="categoryDescriptionRaw"
      @cancel="toggleEditDescription"
      @save="updateDescription({ original: categoryDescriptionRaw, modified: $event})"
    />

    <v-btn
      text
      data-testid="Category-EditDescriptionBtn"
      v-if="!isEditDescription"
      class="category-description__edit-btn mt-3"
      color="grey darken-2"
      :aria-label="descriptionBtnText"
      @click="toggleEditDescription"
    >
      <v-icon size="14" class="mr-1" left>{{ descriptionBtnIcon }}</v-icon>
      {{ descriptionBtnText }}
    </v-btn>
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
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
export default class CategoryDescription extends Vue {
  isEditDescription: boolean = false

  get categoryDescription () {
    return _get(this, '$store.state.category.category.description.html')
  }

  get categoryDescriptionRaw () {
    return _get(this, '$store.state.category.category.description.text')
  }

  get categoryId () {
    return this.$store.state.category.category.id
  }

  get descriptionBtnIcon () {
    return this.categoryDescription ? '$vuetify.icons.pen' : '$vuetify.icons.plus'
  }

  get descriptionBtnText () {
    return this.categoryDescription ? 'Edit description' : 'Add description'
  }

  toggleEditDescription () {
    this.isEditDescription = !this.isEditDescription
  }

  @CatchConflictDecorator
  async updateDescription ({ original, modified }) {
    await this.$store.dispatch('category/updateCategoryDescription', {
      id: this.categoryId,
      original,
      modified
    })
    this.toggleEditDescription()
    this.$store.dispatch('category/reloadCategory')
  }
}
</script>

<style lang="postcss" scoped>
.category-description {
  margin: 0 0 30px;
}
.category-description__content > :first-child {
  margin-top: 0;
}
.category-description__content > :last-child {
  margin-bottom: 0;
}
.category-description__edit-btn {
  background: #e6e9eaa8;
}
</style>

