<template>
  <v-dialog
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
        >
          <!-- v-if="value" - cause without it autofocus triggers on first modal open
          https://stackoverflow.com/questions/51472947/vuetifys-autofocus-works-only-on-first-modal-open -->
          <v-text-field
            v-if="value"
            autofocus
            class="mb-2"
            label="Item name"
            :rules="itemValidationRules"
            v-model="itemName"
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
          @click.native="submit"
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
import { Vue, Component, Prop, Watch } from 'vue-property-decorator'

@Component
export default class AddItemDialog extends Vue {
  @Prop(Boolean) value!: boolean
  @Prop(String) categoryId!: string

  itemName: string = ''

  itemValidationRules: Array<(x: string) => boolean | string> = [
    (x: string) => !!x || 'Item name can not be empty'
  ]

  isValid: boolean = false

  @Watch('value')
  onOpen () {
    this.itemName = ''
  }

  close () {
    this.$emit('input', false)
  }

  async submit () {
    const createdId = await this.$store.dispatch('categoryItem/createItem', {
      category: this.categoryId,
      name: this.itemName
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
