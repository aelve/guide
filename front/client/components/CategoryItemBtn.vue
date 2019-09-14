<template>
  <v-btn
    class="category-item-btn"
    v-tooltip="titleTooltip ? title : undefined"
    v-bind="{
        color: 'grey darken-2',
        icon: !!icon && !showTitle,
        style,
        'aria-label': title,
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

@Component
export default class CategoryItemBtn extends Vue {
  @Prop(String) icon: string
  @Prop(Boolean) small: boolean
  @Prop(String) size: string
  @Prop(String) iconSize: string
  @Prop(String) title: string
  @Prop(Boolean) showTitle: boolean
  @Prop(Boolean) titleTooltip: boolean

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

<style>
.category-item-btn {
  text-transform: none;
}
</style>