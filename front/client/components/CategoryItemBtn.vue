<template>
  <v-btn
    v-bind="{
        color: 'grey darken-2',
        icon: !!icon && !showTitle,
        style,
        title,
        ...$attrs
    }"
    v-on="$listeners"
  >
    <v-icon
      v-if="icon"
      color="grey darken-2"
      :class="{ 'mr-1': showTitle }"
      :size="iconSizeValue"
    >{{ iconText }}</v-icon>
    <template v-if="showTitle"> {{ title }} </template>
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
  @Prop(String) title: string
  @Prop(Boolean) showTitle: boolean

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
