<template>
  <v-dialog
    lazy
    :value="value"
    @input="close"
    @keyup.esc.native="close"
    max-width="500px"
  >
    <slot slot="activator" />
   
    <v-card>
      <v-card-text>
        <v-form
          lazy-validation
          v-model="isValid"
          @keydown.native.prevent.enter="submit"
          ref="form"
        >
          <!-- v-if="value" - cause without it autofocus triggers on first modal open
          https://stackoverflow.com/questions/51472947/vuetifys-autofocus-works-only-on-first-modal-open -->
          <v-text-field
            v-if="value"
            autofocus
            class="mb-2"
            label="Item name"
            :rules="itemValidationRules"
            v-model="name"
          />
          <v-text-field
            class="mb-2"
            label="Name on Hackage (optional)"
            v-model="hackage"
          />
          <v-text-field
            class="mb-2"
            label="Link to the official site, if exists"
            v-model="link"
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
          title="Create"
          :disabled="!isValid || !name"
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
  itemValidationRules: Array<(x: string) => boolean | string> = [
    (x: string) => !!x || 'Item name can not be empty'
  ]
  hackage: string = ''
  link: string = ''
  isValid: boolean = false

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
