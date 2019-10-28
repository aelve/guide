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
            data-testid="CategorySettings-TitleInput"
            :rules="inputValidationRules"
            v-model="title"
          />
          <v-text-field
            class="mb-2"
            label="Group"
            data-testid="CategorySettings-GroupInput"
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
            class="category-settings-dialog__checkbox"
            data-testid="CategorySettings-ItemTraitsSectionCheckbox"
            label="Pros/cons section"
            value="ItemProsConsSection"
            :inputValue="sections"
            @click.native.capture.prevent.stop="updateSectionEnabling('ItemProsConsSection', 'Pros/Cons')"
          />
          <v-checkbox
            hide-details
            color="info"
            class="category-settings-dialog__checkbox"
            data-testid="CategorySettings-ItemEcosystemSectionCheckbox"
            label="Ecosystem section"
            value="ItemEcosystemSection"
            :inputValue="sections"
            @click.native.capture.prevent.stop="updateSectionEnabling('ItemEcosystemSection', 'Ecosystem')"
          />
          <v-checkbox
            hide-details
            color="info"
            class="category-settings-dialog__checkbox"
            data-testid="CategorySettings-ItemNotesSectionCheckbox"
            label="Notes section"
            value="ItemNotesSection"
            :inputValue="sections"
            @click.native.capture.prevent.stop="updateSectionEnabling('ItemNotesSection', 'Notes')"
          />
        </v-form>
      </v-card-text>
      <v-card-actions>
        <v-spacer />
        <v-btn
          text
          aria-label="Cancel"
          color="primary"
          @click.native="close"
        >
          Cancel
        </v-btn>
        <v-btn
          color="info"
          aria-label="Submit"
          data-testid="CategorySettings-SubmitBtn"
          :disabled="!isValid || !hasChanges"
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
import _isEqual from 'lodash/isEqual'
import _sortBy from 'lodash/sortBy'

@Component
export default class CategorySettingsDialog extends Vue {
  @Prop(Boolean) value!: boolean

  title: string = ''
  group: string = ''
  categoryStatus: object = {}
  sections: any[] = []
  initialSettings = {
    title: null,
    group: null,
    categoryStatus: null,
    sections: null
  }
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

  get hasChanges () {
    const properties = ['title', 'group', 'sections', 'categoryStatus']
    return properties
      .some(x => {
        const sortIfArray = (val) => Array.isArray(val) ? _sortBy(val) : val
        return !_isEqual(sortIfArray(this[x]), sortIfArray(this.initialSettings[x]))
      })
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
    this.sections = category.sections.slice()
    this.categoryStatus = category.status

    this.initialSettings = {
      title: this.title,
      group: this.group,
      categoryStatus: this.categoryStatus,
      sections: this.sections.slice()
    }

    this.sectionDisableWarningAgreed = {
      ItemProsConsSection: false,
      ItemEcosystemSection: false,
      ItemNotesSection: false
    }
  }

  // TODO refactor, rewrite simpler
  async updateSectionEnabling (sectionValue, sectionName) {
    const index = this.sections.indexOf(sectionValue)
    const isSectionDisabled = index !== -1
    if (!isSectionDisabled) {
      this.sections.push(sectionValue)
      return
    }

    const isOriginallyEnabled = this.category.sections.includes(sectionValue)
    const isSectionInEdit = this.$store.state.category.itemsSectionsInEdit[sectionValue].length
    const wasAgreedOnce = this.sectionDisableWarningAgreed[sectionValue]
    if (isOriginallyEnabled && isSectionInEdit && !wasAgreedOnce) {
      const isConfirmed = await this._confirm({
        fullText: `You have unsaved changes in one of items’ ${sectionName} section. If you disable this section, your changes will be lost.`
      })
      if (!isConfirmed) {
        return
      }
      this.sectionDisableWarningAgreed[sectionValue] = true
    }

    this.sections.splice(index, 1)
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
      sections: this.sections
    })

    await this.$store.dispatch('category/reloadCategory')
    this.close()
  }
}
</script>

<style lang="postcss" scoped>
.category-settings-dialog__checkbox >>> {
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

