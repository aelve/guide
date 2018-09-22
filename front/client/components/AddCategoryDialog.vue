<template>
  <v-dialog
    v-model="isDialogOpen"
    max-width="500px"
  >

    <slot slot="activator" />

    <v-card>
      <v-card-text>
        <v-form
          ref="form"
          lazy-validation
          v-model="isValid"
          @keydown.native.enter="submit"
        >
        <!-- v-if="isDialogOpen" - cause  without it autofocus triggers on first modal open
          https://stackoverflow.com/questions/51472947/vuetifys-autofocus-works-only-on-first-modal-open -->
          <v-text-field
            v-if="isDialogOpen"
            class="mb-2"
            label="Category name"
            autofocus
            :rules="  categoryValidationRules"
            v-model="categoryName"
            ref="categoryNameInput"
          />
          <v-text-field
            v-model="groupNameInternal"
            label="Group"
          />
        </v-form>
      </v-card-text>
      <v-card-actions>
        <v-spacer></v-spacer>
        <v-btn
          flat
          color="primary"
          :disabled="!isValid"
          @click.native="submit"
        >
          Submit
        </v-btn>
        <v-btn
          flat
          color="primary"
          @click.native="isDialogOpen = false"
        >
          Cancel
        </v-btn>
      </v-card-actions>
    </v-card>
  </v-dialog>
</template>

<script lang="ts">
import { Vue, Component, Prop, Watch } from 'vue-property-decorator'
import { CategoryService } from 'client/service/Category'

@Component
export default class AddCategoryDialog extends Vue {
  @Prop(String) groupName!: string

  groupNameInternal: string = this.groupName || ''
  categoryName: string = ''
  categoryValidationRules: Function[] = [
    (x: string) => !!x || `Category name can't be empty`
  ]
  isDialogOpen: boolean = false
  isValid: boolean = false

  @Watch('groupName')
  onGroupNameChanged(newVal: string) {
    this.groupNameInternal = newVal
  }

  @Watch('isDialogOpen')
  onDialogOpenHide(isOpen: boolean) {
    isOpen ? this.onOpen() : this.onClose()
  }

  onOpen() {
    this.groupNameInternal = this.groupName || ''
  }

  onClose() {
    this.$refs.form.reset()
  }

  async submit() {
    if (!this.$refs.form.validate()) {
      return
    }
    // TODO move to vuex
    // TODO handle errors
    const createdId = await CategoryService.createCategory({
      title: this.categoryName,
      group: this.groupNameInternal
    })
    this.$store.dispatch('category/loadCategoryList')
    window.open(`http://aelve.com:4801/haskell/${createdId}`, '_blank')
    this.isDialogOpen = false
  }
}
</script>

<style>
</style>
