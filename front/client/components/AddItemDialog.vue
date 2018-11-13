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
          @keydown.native.enter="submit"
        >
          <v-text-field 
            class="mb-2"
            label="Item name"
            autofocus
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
import Vue from 'vue'
import Component from 'vue-class-component';
import { Prop, Watch } from 'vue-property-decorator';

@Component
export default class AddItemDialog extends Vue {
  @Prop(Boolean) value!: boolean

  itemName: string = ''

  itemValidationRules: Function[] = [
    (x: string) => !!x || 'Item name can not be empty'
  ]

  isValid: boolean = false
  
  @Watch('value')
  onOpen(newVal: boolean) {
    this.itemName = ''
  } 

  close() {
    this.$emit('input', false)
  }

  async submit() {
    await this.$store.dispatch('categoryItem/createItem', {
      category: 'sth6l9jl',
      name: this.itemName
    })

    this.close()
  }
}
</script>
