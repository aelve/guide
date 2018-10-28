<template>
  <router-link
    class="logo"
    :to="{ name: 'Index' }"
  >
    Aelve Guide
  </router-link>
</template>

<script lang="ts">
import { Vue, Component } from 'vue-property-decorator'

@Component
export default class Logo extends Vue {}
</script>


<style scoped>
.logo {
  text-decoration: none;
  color: #ffffff;
}
</style>
