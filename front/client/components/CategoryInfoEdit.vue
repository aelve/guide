<template>
  <v-dialog
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
          @keydown.native.prevent.enter="submit"
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
            v-model="checkboxSections"
            label="Pros/cons enabled"
            value="ItemProsConsSection"
            class="mt-0 hide-v-messages"
          />
          <v-checkbox 
            v-model="checkboxSections"
            label="Ecosystem field enabled"
            value="ItemEcosystemSection"
            class="mt-0 hide-v-messages"
          />
          <v-checkbox 
            v-model="checkboxSections"
            label="Notes field enabled"
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
import { Prop, Watch } from 'vue-property-decorator';
import { CategoryStatus } from 'client/service/Category'

@Component
export default class CategoryInfoEdit extends Vue {
  @Prop(Boolean) value!: boolean
  @Prop(String) categoryId!: string

  title: string = ''
  group: string = ''
  categoryStatus: object = {}
  checkboxSections: any[] = []
  isValid: boolean = false

  categoryStatuses = [
    { name: 'Complete', value: 'CategoryFinished' }, 
    { name: 'Work in progress', value: 'CategoryWIP' }, 
    { name: 'Stub', value: 'CategoryStub' }
  ]

  inputValidationRules: Function[] = [
    (x: string) => !!x || 'Input can not be empty'
  ]

  @Watch('value')
  onOpen () {
    const category = this.$store.state.category.category
    this.title = category.title
    this.group = category.group
    this.checkboxSections = category.sections
    this.categoryStatus = { name: this.transformCategoryStatus(category.status), value: category.status }
  }

  transformCategoryStatus (status: string) {
    switch(status) {
      case CategoryStatus.finished:
        return 'Complete'
      case CategoryStatus.inProgress:
        return 'Work in progress'
      default:
        return 'Stub'
    }
  }

  close () {
    this.$emit('input', false)
  }

  async updateCategoryInfo () {
    await this.$store.dispatch('category/updateCategoryInfo', {
      id: this.categoryId,
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

