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
            :rules="itemValidationRules"
            v-model="title"
          />
          <v-text-field
            autofocus
            class="mb-2"
            label="Group"
            :rules="itemValidationRules"
            v-model="group"
          />
          <v-select 
            :items="items"
            v-model="categoryStatus"
            label="Status"
            class="mb-2"
          />
          <v-checkbox
            v-model="checkboxSections"
            label="Pros/cons enabled"
            class="mb-2"
          />
          <v-checkbox 
            v-model="checkboxSections"
            label="Ecosystem field enabled"
            class="mb-2"
          />
          <v-checkbox 
            v-model="checkboxSections"
            label="Notes field enabled"
            class="mb-2"
          />
        </v-form>
      </v-card-text>
      <v-card-actions>
        <v-spacer />
        <v-btn
          flat
          color="primary"
          class="add-cat-submit"
          :disabled="!isValid"
          @click="updateCategoryInfo"
        >
          Submit
        </v-btn>
        <v-btn
          flat
          color="primary"
          @click.native="close"
        >
          Cancel
        </v-btn>
      </v-card-actions>
    </v-card>
  </v-dialog>
</template>

<script lang="ts">
import { Vue, Component, Prop, Watch } from 'vue-property-decorator';
import { CategoryStatus } from 'client/service/Category'
import _remove from 'lodash/remove'
import _includes from 'lodash/includes'

@Component
export default class CategoryInfoEdit extends Vue {
  @Prop(Boolean) value!: boolean
  @Prop(String) categoryId!: string

  title: string = ''
  group: string = ''
  categoryStatus: string = ''
  checkboxSections: any[] = []
  isValid: boolean = false

  items = ['Complete', 'Work in progress', 'Stub']

  itemValidationRules: Function[] = [
    (x: string) => !!x || 'Item name can not be empty'
  ]

  beforeMount () {
    this.$vuetify.icons.checkboxOff = this.$vuetify.icons.square
  }

  @Watch('value')
  onOpen () {
    const category = this.$store.state.category.category
    this.title = category.title
    this.group = category.group
    this.categoryStatus = this.transformCategoryStatus(category.status)
  }

  transformCategoryStatus (status: string) {
    if (status === 'CategoryStub') {
      return 'Stub'
    } else if (status === 'CategoryWIP') {
      return 'Work in progress'
    } else if (status === 'CategoryFinished') {
      return 'Complete'
    }
  }

  сategoryStatusRaw () {
    switch(this.categoryStatus) {
      case 'Work in progress':
        return CategoryStatus.inProgress
      case 'Complete':
        return CategoryStatus.finished
      default:
        return CategoryStatus.toBeWritten
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
      status: this.сategoryStatusRaw(),
      sections: this.checkboxSections
    })

    await this.$store.dispatch('category/reloadCategory')
    this.close()
  }
}

</script>
