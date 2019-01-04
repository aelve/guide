<template>
  <div class="category-item-section">
    <h3 class="title font-weight-bold mb-1">
      {{title}}
      <category-item-btn
        small
        :title="editBtnTitle"
        :icon="editBtnIcon"
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
import { Vue, Component, Prop } from 'vue-property-decorator'
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
    default: 'fas fa-pencil-alt'
  }) editBtnIcon: string
  @Prop({
    type: String,
    default: 'edit'
  }) editBtnTitle: string
  @Prop(Boolean) customEdit: boolean

  isEdit: boolean = false

  toggleEdit (): void {
    if (this.customEdit) {
      this.$emit('toggleEdit')
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
.category-item-section:not(:last-child) {
  margin-bottom: 15px;
}
>>> h1 {
  font-size: 16px;
  font-weight: 700;
}
</style>
