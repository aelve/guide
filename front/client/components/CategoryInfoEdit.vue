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
            label="Status"
            class="mb-2"
          />
          <v-checkbox
            v-model="prosConsBox"
            label="Pros/cons enabled"
            class="mb-2"
          />
          <v-checkbox 
            v-model="ecosystem"
            label="Ecosystem field enabled"
            class="mb-2"
          />
          <v-checkbox 
            v-model="notes"
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
</template> -->

<script lang="ts">
import { Vue, Component, Prop, Watch } from 'vue-property-decorator';

@Component
export default class CategoryInfoEdit extends Vue {
  @Prop(Boolean) value!: boolean

  title: string = ''
  group: string = ''
  prosConsBox: boolean = false
  ecosystem: boolean = false
  notes: boolean = false
  isValid: boolean = false

  items = ['Complete', 'Work in progress', 'Stub']

  itemValidationRules: Function[] = [
    (x: string) => !!x || 'Item name can not be empty'
  ]

  @Watch('value')
  onOpen () {
    this.title = ''
  }

  close () {
    this.$emit('input', false)
  }

}

</script>
