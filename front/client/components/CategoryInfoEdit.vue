<template>
  <v-dialog
    :value="value"
    @input="close"
    max-width="500px"
  >
    
    <template
      v-if="$slots.activator"
      v-slot:activator="{ on }"
    >
      <slot
        slot="activator"
        v-on="on"
      />
    </template>


    <v-card>
      <v-card-text>
        <v-form
          v-model="isValid"
          @keydown.native.prevent.enter="updateCategoryInfo"
        >
          <!-- reason for "v-if" - see AddCategoryDialog.vue template-->
          <v-text-field
            v-if="value"
            autofocus
            class="mb-2"
            label="Title"
            :rules="inputValidationRules"
            v-model="title"
          />
          <v-text-field
            class="mb-2"
            label="Group"
            :rules="inputValidationRules"
            v-model="group"
          />
          <v-select
            item-text="name"
            item-value="value"
            :menu-props="{ offsetY: true, closeOnClick: true }"
            :items="categoryStatuses"
            v-model="categoryStatus"
            label="Status"
            class="mb-2"
          />
          <v-checkbox
            hide-details
            color="info"
            class="category-info-edit__checkbox"
            label="Pros/cons section"
            value="ItemProsConsSection"
            :inputValue="checkboxSections"
            @click.native.capture.prevent.stop="updateSectionEnabling('ItemProsConsSection', 'Pros/Cons')"
          />
          <v-checkbox
            hide-details
            color="info"
            class="category-info-edit__checkbox"
            label="Ecosystem section"
            value="ItemEcosystemSection"
            :inputValue="checkboxSections"
            @click.native.capture.prevent.stop="updateSectionEnabling('ItemEcosystemSection', 'Ecosystem')"
          />
          <v-checkbox
            hide-details
            color="info"
            class="category-info-edit__checkbox"
            label="Notes section"
            value="ItemNotesSection"
            :inputValue="checkboxSections"
            @click.native.capture.prevent.stop="updateSectionEnabling('ItemNotesSection', 'Notes')"
          />
        </v-form>
      </v-card-text>
      <v-card-actions>
        <v-spacer />
        <v-btn
          text
          title="Cancel"
          color="primary"
          @click.native="close"
        >
          Cancel
        </v-btn>
        <v-btn
          color="info"
          title="Submit"
          :disabled="!isValid"
          @click="updateCategoryInfo"
        >
          Submit
        </v-btn>
      </v-card-actions>
    </v-card>
  </v-dialog>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'

@Component
export default class CategoryInfoEdit extends Vue {
  @Prop(Boolean) value!: boolean

  title: string = ''
  group: string = ''
  categoryStatus: object = {}
  checkboxSections: any[] = []
  isValid: boolean = false

  // TODO replace ItemProsConsSection and other to constants
  sectionDisableWarningAgreed = {
    ItemProsConsSection: false,
    ItemEcosystemSection: false,
    ItemNotesSection: false
  }

  categoryStatuses = [
    { name: 'Complete', value: 'CategoryFinished' },
    { name: 'Work in progress', value: 'CategoryWIP' },
    { name: 'Stub', value: 'CategoryStub' }
  ]

  inputValidationRules: Function[] = [
    (x: string) => !!x || 'Input can not be empty'
  ]

  get category () {
    return this.$store.state.category.category
  }

  @Watch('value')
  onOpen () {
    const { category } = this
    if (!this.category) {
      this.close()
      return
    }
    this.title = category.title
    this.group = category.group
    this.checkboxSections = category.sections.slice()
    this.categoryStatus = category.status
    this.sectionDisableWarningAgreed = {
      ItemProsConsSection: false,
      ItemEcosystemSection: false,
      ItemNotesSection: false
    }
  }

  async updateSectionEnabling (sectionValue, sectionName) {
    const index = this.checkboxSections.indexOf(sectionValue)
    const isSectionRemoved = index !== -1
    if (!isSectionRemoved) {
      this.checkboxSections.push(sectionValue)
      return
    }

    const isOriginallyEnabled = this.category.sections.includes(sectionValue)
    const isSectionInEdit = this.$store.state.category.itemsSectionsInEdit[sectionValue].length
    const wasAgreedOnce = this.sectionDisableWarningAgreed[sectionValue]
    if (isOriginallyEnabled && isSectionInEdit && !wasAgreedOnce) {
      const isConfirmed = await this._confirm({ fullText: `You have unsaved changes in one of items’ ${sectionName} section. If you disable this section, your changes will be lost.` })
      if (!isConfirmed) {
        return
      }
      this.sectionDisableWarningAgreed[sectionValue] = true
    }

    this.checkboxSections.splice(index, 1)
  }

  close () {
    this.$emit('input', false)
  }

  async updateCategoryInfo () {
    await this.$store.dispatch('category/updateCategoryInfo', {
      id: this.category.id,
      title: this.title,
      group: this.group,
      status: this.categoryStatus,
      sections: this.checkboxSections
    })

    await this.$store.dispatch('category/reloadCategory')
    this.close()
  }
}
</script>

<style lang="postcss" scoped>
.category-info-edit__checkbox >>> {
  .v-input--selection-controls__input {
    margin-right: 12px;
  }

  /* We using fontawesome icons which turned out to be bigger than vuetify default icons
     and checkbox icon gets some strange offset if overflow is visible */
  svg {
    overflow: hidden;
  }
}
</style>

