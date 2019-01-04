<template>
  <category-item-section
    customEdit
    editBtnIcon="fas fa-plus"
    editBtnTitle="add"
    :title="title"
    @toggleEdit="toggleAddTrait"
  >
    <ul v-if="traitsModel && traitsModel.length">
      <li
        v-for="(trait, index) in traitsModel"
        :key="index"
        class="position-relative category-item-trait"
      >

        <div
          v-html="trait.content.html"
          v-if="!trait.isEdit"
        />

        <v-menu
          v-if="!trait.isEdit"
          lazy
          offset-y
          nudge-bottom="2"
          class="category-item-edit-trait-menu"
        >
          <category-item-btn
            slot="activator"
            size="22px"
            :iconSize="22"
            icon="more_vert"
          />
          <v-list dense>
            <v-list-tile
              tag="div"
              @click="moveTrait(trait, 'up')"
            >
              <v-list-tile-title>
                Move up
              </v-list-tile-title>
            </v-list-tile>

            <v-list-tile
              tag="div"
              @click="moveTrait(trait, 'down')"
            >
              <v-list-tile-title>
                Move down
              </v-list-tile-title>
            </v-list-tile>

            <v-list-tile
              tag="div"
              @click="trait.isEdit = true"
            >
              <v-list-tile-title>
                Edit
              </v-list-tile-title>
            </v-list-tile>

            <v-list-tile
              tag="div"
              @click="deleteTrait(trait)"
            >
              <v-list-tile-title>
                Delete
              </v-list-tile-title>
            </v-list-tile>
          </v-list>
        </v-menu>

        <markdown-editor
          v-if="trait.isEdit"
          saveOnEnter
          :value="trait.content.text"
          :height="100"
          @cancel="trait.isEdit = false"
          @save="saveEdit(trait, $event)"
        />
      </li>
    </ul>

    <markdown-editor
      v-if="isAddTrait"
      saveOnEnter
      :height="100"
      @cancel="toggleAddTrait"
      @save="createTrait"
    />
  </category-item-section>
</template>

<script lang="ts">
import { Vue, Component, Prop, Watch } from 'vue-property-decorator'
import _cloneDeep from 'lodash/cloneDeep'
import CategoryItemSection from 'client/components/CategoryItemSection.vue'
import CategoryItemBtn from 'client/components/CategoryItemBtn.vue'
import MarkdownEditor from 'client/components/MarkdownEditor.vue'

@Component({
  components: {
    CategoryItemSection,
    CategoryItemBtn,
    MarkdownEditor
  }
})
export default class CategoryItemTraits extends Vue {
  // TODO change [any] type
  @Prop(Array) traits!: any[]
  @Prop(String) type!: string
  @Prop(String) itemId: string

  isEdit: boolean = false
  isAddTrait: boolean = false
  traitsModel: any[] = []

  get title () {
    return this.type === 'pro' ? 'Pros' : 'Cons'
  }

  @Watch('traits', {
    immediate: true
  })
  setTraitsModel (traits: any[]) {
    this.traitsModel = _cloneDeep(traits)
    this.traitsModel.forEach(x => this.$set(x, 'isEdit', false))
  }

  toggleEdit () {
    this.isEdit = !this.isEdit
  }

  async saveEdit (trait: any, modifiedText: string) {
    await this.$store.dispatch('categoryItem/updateItemTrait', {
      itemId: this.itemId,
      traitId: trait.uid,
      original: trait.content.text,
      modified: modifiedText
    })
    trait.isEdit = false
    await this.$store.dispatch('category/reloadCategory')
  }

  async moveTrait (trait: any, direction: string) {
    await this.$store.dispatch('categoryItem/moveItemTrait', {
      itemId: this.itemId,
      traitId: trait.uid,
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
      text: traitText
    })
    this.toggleAddTrait()
    await this.$store.dispatch('category/reloadCategory')
  }

  async deleteTrait (trait: any) {
    // TODO confirmation
    await this.$store.dispatch('categoryItem/deleteItemTrait', {
      itemId: this.itemId,
      traitId: trait.uid
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
  margin-bottom: 2px;
}
.category-item-edit-trait-menu {
  position: absolute;
  top: 0;
  right: 0;
}
>>> .v-list__tile {
  padding: 0 8px;
}
</style>