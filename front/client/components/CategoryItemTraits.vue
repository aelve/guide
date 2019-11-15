<template>
  <category-item-section
    customEdit
    editBtnIcon="plus"
    editBtnTitle="Add"
    :title="title"
    @toggleEdit="toggleAddTrait"
  >
    <ul v-if="traitsModel && traitsModel.length">
      <li
        v-for="trait in traitsModel"
        :key="trait.id"
        class="position-relative category-item-trait"
      >

        <template v-if="!trait.isEdit">
          <div v-html="trait.content.html" />

          <v-menu bottom left offset-y>
            <template v-slot:activator="{ on }">
              <category-item-btn
                titleTooltip
                class="trait-actions-menu-btn"
                size="22px"
                title="Actions"
                iconSize="14"
                icon="ellipsis-v"
                v-on='on'
              />
            </template>
            <v-list dense>
              <v-list-item
                tag="button"
                @click="moveTrait(trait, 'up')"
              >
                <v-list-item-title>
                  Move up
                </v-list-item-title>
              </v-list-item>

              <v-list-item
                tag="button"
                @click="moveTrait(trait, 'down')"
              >
                <v-list-item-title>
                  Move down
                </v-list-item-title>
              </v-list-item>

              <v-list-item
                tag="button"
                @click="trait.isEdit = true"
              >
                <v-list-item-title>
                  Edit
                </v-list-item-title>
              </v-list-item>

              <v-list-item
                tag="button"
                @click="deleteTrait(trait)"
              >
                <v-list-item-title>
                  Delete
                </v-list-item-title>
              </v-list-item>
            </v-list>
          </v-menu>
        </template>

        <markdown-editor
          v-if="trait.isEdit"
          saveOnEnter
          :value="trait.content.text"
          :height="60"
          @cancel="trait.isEdit = false"
          @save="saveEdit({trait, original: trait.content.text, modified: $event})"
        />
      </li>
    </ul>

    <markdown-editor
      v-if="isAddTrait"
      saveOnEnter
      :height="60"
      @cancel="toggleAddTrait"
      @save="createTrait"
    />
  </category-item-section>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import Confirm from 'client/helpers/ConfirmDecorator'
import CategoryItemSection from 'client/components/CategoryItemSection.vue'
import CategoryItemBtn from 'client/components/CategoryItemBtn.vue'
import MarkdownEditor from 'client/components/MarkdownEditor.vue'
import conflictDialogMixin from 'client/mixins/conflictDialogMixin'
import CatchConflictDecorator from 'client/helpers/CatchConflictDecorator'

@Component({
  components: {
    CategoryItemSection,
    CategoryItemBtn,
    MarkdownEditor
  },
  mixins: [conflictDialogMixin]

})
export default class CategoryItemTraits extends Vue {
  @Prop(Array) traits!: any[]
  @Prop(String) type!: string
  @Prop(String) itemId: string

  isAddTrait: boolean = false
  traitsModel = []

  get title () {
    return this.type + 's'
  }

  get isAnyTraitEditing () {
    return this.traitsModel.some(x => x.isEdit) || this.isAddTrait
  }

  @Watch('isAnyTraitEditing')
  updateIsEditing (newVal) {
    this.$emit('update:isAnyTraitEditing', newVal)
  }

  @Watch('traits', { immediate: true })
  setTraitsModel (traits) {
    const editingTraitIds = this.traitsModel.filter(x => x.isEdit).map(x => x.id)
    // Destructering into new object to avoid prop mutation when adding new property later ("isEdit")
    this.traitsModel = traits.map(x => ({ ...x }))
    this.traitsModel.forEach(x => this.$set(x, 'isEdit', editingTraitIds.includes(x.id)))
  }

  @CatchConflictDecorator
  async saveEdit ({ trait, original, modified }) {
    await this.$store.dispatch('categoryItem/updateItemTrait', {
      itemId: this.itemId,
      traitId: trait.id,
      original,
      modified
    })
    trait.isEdit = false
    await this.$store.dispatch('category/reloadCategory')
  }

  async moveTrait (trait: any, direction: string) {
    await this.$store.dispatch('categoryItem/moveItemTrait', {
      itemId: this.itemId,
      traitId: trait.id,
      direction
    })
    await this.$store.dispatch('category/reloadCategory')
  }

  toggleAddTrait () {
    this.isAddTrait = !this.isAddTrait
  }

  async createTrait (traitText: string) {
    await this.$store.dispatch('categoryItem/createItemTrait', {
      itemId: this.itemId,
      type: this.type,
      content: traitText
    })
    this.toggleAddTrait()
    await this.$store.dispatch('category/reloadCategory')
  }

  @Confirm({
    text: 'delete this trait',
    confirmBtnText: 'Delete',
    confirmBtnProps: {
      color: 'error'
    }
  })
  async deleteTrait (trait: any) {
    await this.$store.dispatch('categoryItem/deleteItemTrait', {
      itemId: this.itemId,
      traitId: trait.id
    })
    await this.$store.dispatch('category/reloadCategory')
  }
}
</script>

<style scoped>
.category-item-traits {
  display: flex;
}

.category-item-traits > * {
  flex: 1;
}

.category-item-traits > *:not(:last-child) {
  margin-right: 20px;
}
.category-item-trait {
  padding-right: 24px;
}
.trait-actions-menu-btn {
  position: absolute;
  top: 0;
  right: 0;
}
>>> .v-list-item {
  padding: 0 8px;
  width: 100%;
  text-align: left;
}
</style>