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
            :rules="categoryValidationRules"
            v-model="categoryName"
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
          text
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

    <!--
      `eager` prop is provided here because dialog doesn't get focus on first render without it
      which leads to inability of closing it with `esc` btn
      TODO fix, when issue is fixed in vuetify https://github.com/vuetifyjs/vuetify/issues/8220
    -->
    <ConfirmDialog
      eager
      ref="duplicateConfirm"
      max-width="500px"
      :value="isDuplicateConfirmShow"
    >
      This group already has categories with the same name:
      <ul class="duplicate-categories-list">
        <li
          v-for="category in sameNameCategories"
          :key="category.id"
        >
          <a target="_blank">{{ category.title }}</a>
        </li>
      </ul>
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

