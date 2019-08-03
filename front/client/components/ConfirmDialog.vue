<!-- Universal confirmation dialog, just pass text and function in Props -->
<template>
  <v-dialog
    :value="value"
    :attach="attach"
    max-width="500px"
    v-bind="$attrs"
    @input="cancel"
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
      <v-card-text class="confirm-dialog__text">
        <slot v-if="$slots.default"/>

        <template v-else>
          {{ fullText || `Are you sure you want to ${text}?` }}
        </template>
      </v-card-text>

      <v-divider />
      <v-card-actions>
        <v-spacer />
        <v-btn
          v-bind="{
            text: true,
            color:'primary',
            title: cancelBtnText,
            ...cancelBtnProps
          }"
          @click.native="cancel"
        >
          {{ cancelBtnText }}
        </v-btn>
        <v-btn
          v-bind="{
            color:'info',
            title: confirmBtnText,
            ...confirmBtnProps
          }"
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

@Component({
  inheritAttrs: false
})
export default class ConfirmDialog extends Vue {
  @Prop(String) text!: string
  @Prop(String) fullText!: string
  @Prop(Boolean) value!: boolean
  @Prop(String) attach!: string
  @Prop({ default: 'Continue' }) confirmBtnText!: string
  @Prop({ default: 'Cancel' }) cancelBtnText!: string
  @Prop(Object) confirmBtnProps!: object
  @Prop(Object) cancelBtnProps!: object

  close () {
    this.$emit('input', false)
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

<style scoped>
.v-card__text.confirm-dialog__text {
  font-size: 1rem;
  line-height: inherit;
  color: #000;
}
</style>

