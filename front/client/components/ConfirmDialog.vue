<!-- Universal confirmation dialog, just pass text and function in Props -->
<template>
  <v-dialog
    :value="value"
    @input="close"
    max-width="500px"  
  >
    <slot slot="activator" />

    <v-card>
      <v-card-text>
        Are you sure you want to {{ confirmationText }}
      </v-card-text>
      <v-divider />
      <v-card-actions>
        <v-spacer />
        <v-btn 
          flat
          color="primary"
          class="confirm-btn"
          @click.native="confirmAction(itemId); close"
        >
          Continue
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
import { Prop } from 'vue-property-decorator';

@Component
export default class ConfirmDialog extends Vue {
  @Prop(String) confirmationText!: string
  @Prop(Function) confirmAction!: Function
  @Prop(Boolean) value!: boolean
  @Prop(String) itemId!: string

  close() {
    this.$emit('input', false)
  }
}

</script>
