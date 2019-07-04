<!-- Universal confirmation dialog, just pass text and function in Props -->
<template>
  <v-dialog
    :value="value"
    :attach="attach"
    max-width="500px"
    @input="close"
  >
    <slot slot="activator" />

    <v-card>
      <v-card-text v-if="$slots.default">
        <slot />
      </v-card-text>
      <v-card-text v-else>
        {{ fullText || `Are you sure you want to ${text} ?` }}
      </v-card-text>
      <v-divider />
      <v-card-actions>
        <v-spacer />
        <v-btn
          flat
          :title="cancelBtnText"
          color="primary"
          @click.native="cancel"
        >
          {{ cancelBtnText }}
        </v-btn>
        <v-btn
          flat
          :title="confirmBtnText"
          color="primary"
          @click.native="confirm"
        >
          {{ confirmBtnText }}
        </v-btn>
      </v-card-actions>
    </v-card>
  </v-dialog>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'

@Component
export default class ConfirmDialog extends Vue {
  @Prop(String) text!: string
  @Prop(String) fullText!: string
  @Prop(Boolean) value!: boolean
  @Prop(String) attach!: string
  @Prop({ default: 'Continue' }) confirmBtnText!: string
  @Prop({ default: 'Cancel' }) cancelBtnText!: string

  close () {
    this.$emit('input', false)
    this.$emit('canceled')
  }
  confirm () {
    this.$emit('confirmed')
    this.close()
  }
  cancel () {
    this.$emit('canceled')
    this.close()
  }
}
</script>
