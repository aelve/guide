<template>
  <v-dialog
    :value="value"
    @input="close"
    max-width="500px"
  >

    <slot slot="activator" />

    <v-card @keyup.esc.native="close" tabindex="0">
      <v-card-text>
        <v-form
          ref="form"
          lazy-validation
          v-model="isValid"
          @keydown.native.enter="submit"
        >
        <!-- v-if="value" - cause without it autofocus triggers on first modal open
          https://stackoverflow.com/questions/51472947/vuetifys-autofocus-works-only-on-first-modal-open -->
          <v-text-field
            v-if="value"
            class="mb-3"
            label="Category name"
            autofocus
            :rules="categoryValidationRules"
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
          @click.native="close"
        >
          Cancel
        </v-btn>
        <v-btn
          flat
          color="primary"
          class="add-category-submit-btn"
          :disabled="!isValid"
          @click.native="submit"
        >
          Submit
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
  @Prop(Boolean) value!: boolean

  groupNameInternal: string = this.groupName || ''
  categoryName: string = ''
  categoryValidationRules: Function[] = [
    (x: string) => !!x || `Category name can't be empty`
  ]
  isValid: boolean = false

  @Watch('value')
  onOpen (newVal: boolean) {
    this.categoryName = ''
    this.groupNameInternal = this.groupName
  }

  close () {
    this.$emit('input', false)
  }

  async submit () {
    if (!this.$refs.form.validate()) {
      return
    }
    const createdId = await this.$store.dispatch('category/createCategory', {
      title: this.categoryName,
      group: this.groupNameInternal
    })
    window.open(`http://guide.aelve.com:4801/haskell/${createdId}`, '_blank')
    this.close()
  }
}
</script>

<style>
</style>
