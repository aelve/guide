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
            :items="$options.categoryStatuses"
            v-model="categoryStatus"
            label="Status"
            class="mb-2"
          />
          <v-checkbox
            v-for="section of $options.categorySections"
            :key="section.value"
            hide-details
            color="info"
            class="category-settings-dialog__checkbox"
            data-testid="CategorySettings-ItemTraitsSectionCheckbox"
            :label="section.name"
            :value="section.value"
            :inputValue="sections"
            @click.native.capture.prevent.stop="updateSectionEnabling(section.value, section.name)"
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
import { CategoryStatus, CategorySection, ICategoryFull } from 'client/service/Category'

const categoryStatusNames = {
  [CategoryStatus.finished]: 'Complete',
  [CategoryStatus.inProgress]: 'Work in progress',
  [CategoryStatus.toBeWritten]: 'Stub'
}

const categorySectionNames = {
  [CategorySection.prosCons]: 'Pros/cons section',
  [CategorySection.ecosystem]: 'Ecosystem section',
  [CategorySection.notes]: 'Notes section'
}

const categoryStatuses = Object.entries(categoryStatusNames)
  .map(([value, name]) => ({ value, name }))

const categorySections = Object.entries(categorySectionNames)
  .map(([value, name]) => ({ value, name }))

@Component({
  // categoryStatuses, categorySections is here to use in template but without reactivity
  categoryStatuses,
  categorySections
})
export default class CategorySettingsDialog extends Vue {
  @Prop(Boolean) value: boolean
  @Prop(Object) category: ICategoryFull

  title: string = null
  group: string = null
  categoryStatus: CategoryStatus = null
  sections: CategorySection[] = []
  initialSettings = {
    title: null,
    group: null,
    categoryStatus: null,
    sections: null
  }
  isValid: boolean = false
  isSectionDisableAgreed = this.isSetSectionDisableAgreedInit()
  inputValidationRules: Array<(x: string) => boolean | string> = [
    (x: string) => !!x || 'Input can not be empty'
  ]

  get hasChanges () {
    const properties = ['title', 'group', 'sections', 'categoryStatus']

    return properties
      .some(x => {
        const sortIfArray = (val) => Array.isArray(val) ? _sortBy(val) : val
        const current = sortIfArray(this[x])
        const initial = sortIfArray(this.initialSettings[x])
        return !_isEqual(current, initial)
      })
  }

  @Watch('value')
  onOpen (newVal) {
    if (!newVal) {
      return
    }
    const { category } = this
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

    this.isSectionDisableAgreed = this.isSetSectionDisableAgreedInit()
  }

  isSetSectionDisableAgreedInit () {
    return Object.values(CategorySection).reduce((acc, cur) => {
      acc[cur] = false
      return acc
    }, {})
  }

  async updateSectionEnabling (sectionValue, sectionName) {
    const index = this.sections.indexOf(sectionValue)
    const isSectionDisabled = index !== -1
    if (!isSectionDisabled) {
      this.sections.push(sectionValue)
      return
    }

    const isOriginallyEnabled = this.initialSettings.sections.includes(sectionValue)
    const isSectionInEdit = this.$store.state.category.itemsSectionsInEdit[sectionValue].length
    const wasAgreedBefore = this.isSectionDisableAgreed[sectionValue]
    if (isOriginallyEnabled && isSectionInEdit && !wasAgreedBefore) {
      const confirmText = `You have unsaved changes in one of items ${sectionName}. If you disable this section, your changes will be lost.`
      const isConfirmed = await this._confirm({
        fullText: confirmText
      })
      if (!isConfirmed) {
        return
      }
      this.isSectionDisableAgreed[sectionValue] = true
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

  /* We are using fontawesome icons which turned out to be bigger than vuetify default icons
     and checkbox icon gets some strange offset if overflow is visible */
  svg {
    overflow: hidden;
  }
}
</style>

