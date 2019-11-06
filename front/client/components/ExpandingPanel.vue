<template>
  <div
    class="expanding-panel"
    :class="{ 'expanding-panel_expanded': value }"
    :aria-hidden="!value"
  >
    <slot />
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'

@Component
export default class ExpandingPanel extends Vue {
  @Prop(Boolean) value: boolean
}
</script>

<style scoped>
.expanding-panel {
  max-height: 0;
  overflow: hidden;
  transition: max-height 0.2s ease-in-out;
}
.expanding-panel_expanded {
  max-height: 600px;
}
</style>