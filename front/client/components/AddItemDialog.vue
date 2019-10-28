<template>
  <v-dialog
    :value="value"
    @input="close"
    @keyup.esc.native="close"
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
          @keydown.native.prevent.enter="submit"
        >
          <!-- reason for "v-if" - see AddCategoryDialog.vue template-->
          <v-text-field
            v-if="value"
            autofocus
            class="mb-2"
            label="Item name"
            data-testid="AddItemDialog-NameInput"
            :rules="nameValidationRules"
            v-model="name"
          />
          <v-text-field
            class="mb-2"
            label="Name on Hackage (optional)"
            data-testid="AddItemDialog-HackageInput"
            v-model="hackage"
          />
          <v-text-field
            class="mb-2"
            label="Link to the official site, if exists"
            data-testid="AddItemDialog-LinkInput"
            v-model="link"
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
          :disabled="!isValid"
          data-testid="AddItemDialog-SubmitBtn"
          aria-label="Create"
          @click.native="submit"
        >
          Create
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
export default class AddItemDialog extends Vue {
  @Prop(Boolean) value!: boolean
  @Prop(String) categoryId!: string

  name: string = ''
  nameValidationRules: Array<(x: string) => boolean | string> = [
    (x: string) => !!x || 'Item name can not be empty'
  ]
  hackage: string = ''
  link: string = ''
  isValid: boolean = false

  // TODO create mixin or external dialog component which reset data on new open,
  // cause this code is duplicated in another dialog components (AddCategoryDialog)
  @Watch('value')
  onOpen () {
    this.name = ''
    this.hackage = ''
    this.link = ''
  }

  close () {
    this.$emit('input', false)
  }

  async submit () {
    if (!this.$refs.form.validate()) {
      return
    }

    const createdId = await this.$store.dispatch('categoryItem/createItem', {
      category: this.categoryId,
      name: this.name,
      hackage: this.hackage || null,
      link: this.link || null
    })
    await this.$store.dispatch('category/reloadCategory')
    this.close()
    // nextTick to wait for item rendered in dom so router can find it and scroll to it
    this.$nextTick(() => {
      this.$router.push({ hash: `item-${createdId}` })
    })
  }
}
</script>
