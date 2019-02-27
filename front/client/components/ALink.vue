<template>
  <a
    class="link"
    :target="openInNewTab ? '_blank': ''"
    :rel="openInNewTab ? 'noopener noreferrer': ''"
    :href="url"
  >
    <slot />
  </a>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'

@Component
export default class ALink extends Vue {
  @Prop(Boolean) openInNewTab!: boolean
  @Prop(String) url!: string
}
</script>