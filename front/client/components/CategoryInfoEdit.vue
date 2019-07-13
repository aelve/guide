<template>
  <v-dialog
    lazy
    :value="value"
    @input="close"
    max-width="500px"
  >
    <slot slot="activator" />

    <v-card>
      <v-card-text>
        <v-form
          lazy-validation
          v-model="isValid"
          @keydown.native.prevent.ctrl.enter="updateCategoryInfo"
        >
          <v-text-field
            autofocus
            class="mb-2"
            label="Title"
            :rules="inputValidationRules"
            v-model="title"
          />
          <v-text-field
            autofocus
            class="mb-2"
            label="Group"
            :rules="inputValidationRules"
            v-model="group"
          />
          <v-select
            :items="categoryStatuses"
            item-text="name"
            item-value="value"
            v-model="categoryStatus"
            label="Status"
            class="mb-2"
          />
          <v-checkbox
            :inputValue="checkboxSections"
            @click.native.capture.prevent.stop="updateSectionEnabling('ItemProsConsSection', 'Pros/Cons')"
            label="Pros/cons section"
            value="ItemProsConsSection"
            class="mt-0 hide-v-messages"
          />
          <v-checkbox
            :inputValue="checkboxSections"
            @click.native.capture.prevent.stop="updateSectionEnabling('ItemEcosystemSection', 'Ecosystem')"
            label="Ecosystem section"
            value="ItemEcosystemSection"
            class="mt-0 hide-v-messages"
          />
          <v-checkbox
            :inputValue="checkboxSections"
            @click.native.capture.prevent.stop="updateSectionEnabling('ItemNotesSection', 'Notes')"
            label="Notes section"
            value="ItemNotesSection"
            class="mt-0 hide-v-messages"
          />
        </v-form>
      </v-card-text>
      <v-card-actions>
        <v-spacer />
        <v-btn
          flat
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

<style scoped>
>>> .hide-v-messages .v-messages {
  display: none;
}
</style>

