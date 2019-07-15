<template>
  <div class="category-item-section">
    <h3 class="category-item-section__title title font-weight-bold mb-1">
      {{ title }}
      <category-item-btn
        small
        :title="editBtnTitle"
        :icon="editBtnIcon"
        class="ml-1"
        @click="toggleEdit"
      />
    </h3>

    <slot v-if="!isEdit"/>

    <markdown-editor
      v-else
      class="mb-2"
      toolbar
      :value="editText"
      :height="editorHeight"
      @cancel="toggleEdit"
      @save="save"
    />
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import MarkdownEditor from 'client/components/MarkdownEditor.vue'
import CategoryItemBtn from 'client/components/CategoryItemBtn.vue'

@Component({
  components: {
    MarkdownEditor,
    CategoryItemBtn
  }
})
export default class CategoryItemSection extends Vue {
  @Prop({
    type: String,
    default: ''
  }) title: string
  @Prop({
    type: String,
    default: ''
  }) editText: string
  @Prop({
    type: Number,
    default: 200
  }) editorHeight: number
  @Prop({
    type: String,
    default: 'pencil-alt'
  }) editBtnIcon: string
  @Prop({
    type: String,
    default: 'Edit'
  }) editBtnTitle: string
  @Prop(Boolean) customEdit: boolean

  isEdit: boolean = false

  toggleEdit (): void {
    this.$emit('toggleEdit')
    if (this.customEdit) {
      return
    }
    this.isEdit = !this.isEdit
  }

  save (edited: string): void {
    this.$emit('save', edited)
    this.toggleEdit()
  }
}
</script>


<style scoped>
.category-item-section__title {
  display: flex;
  align-items: center;
}
</style>
