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
          ref="form"
          v-model="isValid"
          @keydown.native.enter="submit"
        >
        <!-- v-if="value" - cause without it autofocus triggers on first modal open
          https://stackoverflow.com/questions/51472947/vuetifys-autofocus-works-only-on-first-modal-open -->
          <v-text-field
            v-if="value"
            autofocus
            ref="categoryNameInput"
            class="mb-3"
            label="Category name"
            data-testid="AddCategoryDialog-NameInput"
            :rules="categoryValidationRules"
            v-model="categoryName"
          />
          <v-text-field
            data-testid="AddCategoryDialog-GroupInput"
            :rules="groupValidationRules"
            v-model="groupNameInternal"
            label="Group"
          />
        </v-form>
      </v-card-text>
      <v-card-actions>
        <v-spacer></v-spacer>
        <v-btn
          text
          aria-label="Cancel"
          color="primary"
          @click.native="close"
        >
          Cancel
        </v-btn>
        <v-btn
          aria-label="Submit"
          color="info"
          class="add-category-submit-btn"
          data-testid="AddCategoryDialog-SubmitBtn"
          :disabled="!isValid"
          @click.native="submit"
        >
          Submit
        </v-btn>
      </v-card-actions>
    </v-card>

    <!--
      `eager` prop is provided here because dialog doesn't get focus on first render without it
      which leads to inability of closing it with `esc` btn
      TODO fix, when issue is fixed in vuetify https://github.com/vuetifyjs/vuetify/issues/8220
    -->
    <ConfirmDialog
      eager
      ref="duplicateConfirm"
      :confirmBtnProps="{
        'data-testid': 'DuplicateCategoryDialog-ConfirmBtn'
      }"
      max-width="500px"
      :value="isDuplicateConfirmShow"
    >
      <div data-testid="DuplicateCategoryDialog">
        This group already has categories with the same name:
        <ul class="duplicate-categories-list">
          <li
            v-for="category in sameNameCategories"
            :key="category.id"
          >
            <router-link
              :to="`/haskell/${getCategoryUrl(category)}`"
              target="_blank"
            >{{ category.title }}</router-link>
          </li>
        </ul>
      </div>
    </ConfirmDialog>

  </v-dialog>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import { CategoryService } from 'client/service/Category'
import ConfirmDialog from 'client/components/ConfirmDialog.vue'
import DeferredPromise from 'utils/DeferredPromise'
import getCategoryUrl from 'client/helpers/getCategoryUrl'

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
  groupValidationRules: Array<(x: string) => boolean | string> = [
    (x: string) => !!x || `Group can't be empty`
  ]
  isValid: boolean = false
  isDuplicateConfirmShow: boolean = false
  sameNameCategories = []

  get categories () {
    return this.$store.state.category.categoryList
  }

  @Watch('value')
  onToggle (val) {
    if (val) {
      Object.assign(this.$data, this.$options.data.apply(this))
    } else {
      this.$refs.form.resetValidation()
    }
  }

  close () {
    this.$emit('input', false)
  }

  calcSameNameCategories () {
    if (!this.categoryName || !this.groupNameInternal) {
      return []
    }
    return this.categories
      .filter(x => x.group === this.groupNameInternal && x.title === this.categoryName)
  }

  async submit () {
    if (!this.$refs.form.validate()) {
      return
    }
    this.sameNameCategories = this.calcSameNameCategories()
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
    this.close()
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

        // when duplicateConfirm dialog closed it automatically sets focus on <body>
        // and we focus on name input for: so that user could change category name and so that he could push esc to close dialog
        this.$nextTick(() => this.$refs.categoryNameInput.focus())
      })
      duplicateConfirm.$once('confirmed', () => {
        promise.resolve(true)
        this.isDuplicateConfirmShow = false
      })
    })
    return promise
  }

  getCategoryUrl = getCategoryUrl
}
</script>

<style lang="postcss" scoped>
.duplicate-categories-list {
  display: inline;
  margin: 0;
  padding: 0;

  > li {
    list-style-type: none;
    display: inline;

    &:not(:last-child):after {
      content: ", ";
    }
  }
}
</style>

