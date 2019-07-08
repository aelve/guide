<template>
  <v-btn
    :icon="!!icon"
    class="ma-0"
    :style="style"
    v-bind="$attrs"
    v-on="$listeners"
  >
    <v-icon
      v-if="icon"
      color="grey darken-2"
      :size="iconSizeValue"
    >{{ iconText }}</v-icon>
    <slot />
  </v-btn>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import _omit from 'lodash/omit'

@Component
export default class CategoryItemBtn extends Vue {
  @Prop(String) icon: string
  @Prop(Boolean) small: boolean
  @Prop(String) size: string
  @Prop(String) iconSize: string

  get style () {
    // Size prop overlaps small prop
    if (this.size) {
      return { width: this.size, height: this.size }
    }
    return this.small ? { width: '22px', height: '22px' } : {}
  }

  get iconSizeValue () {
    if (this.iconSize) {
      return this.iconSize
    }
    return this.small ? 12 : undefined
  }

  get iconText () {
    return `$vuetify.icons.${this.icon}`
  }
}
</script>
