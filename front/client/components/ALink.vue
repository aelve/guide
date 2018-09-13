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
import { Prop, Component, Vue } from 'vue-property-decorator'

@Component
export default class ALink extends Vue {
  @Prop(Boolean) openInNewTab!: boolean
  @Prop(String) url!: string
}

</script>

<style scoped>
.link {
  text-decoration-line: none;
}
.link:hover {
  text-decoration-line: underline;
}
</style>
