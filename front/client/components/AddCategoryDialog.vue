<template>
  <div>
    <v-dialog
      lazy
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
            title="Cancel"
            color="primary"
            @click.native="close"
          >
            Cancel
          </v-btn>
          <v-btn
            title="Submit"
            color="info"
            class="add-category-submit-btn"
            :disabled="!isValid"
            @click.native="submit"
          >
            Submit
          </v-btn>
        </v-card-actions>
      </v-card>

    </v-dialog>

    <ConfirmDialog
      v-if="isDuplicateConfirmShow"
      :value="isDuplicateConfirmShow"
      max-width="500px"
      attach="#app"
      ref="duplicateConfirm"
    >
      This group already has categories with the same name:
      <a
        v-for="category in sameNameCategories"
        :key="category.id"
        target="_blank"
      >{{ category.title }}</a>
    </ConfirmDialog>
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import { CategoryService } from 'client/service/Category'
import ConfirmDialog from 'client/components/ConfirmDialog.vue'
import DeferredPromise from 'utils/DeferredPromise'

@Component({
  components: {
    ConfirmDialog
  }
})
export default class AddCategoryDialog extends Vue {
  @Prop(String) groupName!: string
  @Prop(Boolean) value!: boolean

  groupNameInternal: string = this.groupName || ''
  categoryName: string = ''
  categoryValidationRules: Array<(x: string) => boolean | string> = [
    (x: string) => !!x || `Category name can't be empty`
  ]
  isValid: boolean = false
  isDuplicateConfirmShow: boolean = false

  get categories () {
    return this.$store.state.category.categoryList
  }

  get sameNameCategories () {
    if (!this.categoryName || !this.groupName) {
      return []
    }
    return this.categories
      .filter(x => x.group === this.groupName && x.title === this.categoryName)
  }

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

    if (this.sameNameCategories.length) {
      const isDuplicationConfirmed = await this.confirmDuplicate()
      if (!isDuplicationConfirmed) {
        return
      }
    }

    const createdId = await this.$store.dispatch('category/createCategory', {
      title: this.categoryName,
      group: this.groupNameInternal
    })
    this.$router.push(`haskell/${createdId}`)
  }

  async confirmDuplicate () {
    const promise = new DeferredPromise()
    this.isDuplicateConfirmShow = true
    this.$nextTick(() => {
      const duplicateConfirm = this.$refs.duplicateConfirm
      duplicateConfirm.$once('canceled', () => {
        promise.resolve(false)
        this.isDuplicateConfirmShow = false
      })
      duplicateConfirm.$once('confirmed', () => {
        promise.resolve(true)
        this.isDuplicateConfirmShow = false
      })
    })
    return promise
  }
}
</script>

<style>
</style>
