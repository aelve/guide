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
          @click.native="confirm"
        >
          {{ confirmBtnText }}
        </v-btn>
        <v-btn
          flat
          color="primary"
          @click.native="cancel"
        >
          {{ cancelBtnText }}
        </v-btn>
      </v-card-actions>
    </v-card>
  </v-dialog>
</template>

<script lang="ts">
import { Vue, Component, Prop } from 'vue-property-decorator'

@Component
export default class ConfirmDialog extends Vue {
  @Prop(String) confirmationText!: string
  @Prop(Boolean) value!: boolean
  @Prop({ default: 'Continue' }) confirmBtnText!: string
  @Prop({ default: 'Cancel' }) cancelBtnText!: string

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
