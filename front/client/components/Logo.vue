<template>
  <router-link
    class="logo"
    :to="{ name: 'Index' }"
  >
    Aelve Guide
  </router-link>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'

@Component
export default class Logo extends Vue {}
</script>


<style scoped>
.logo {
  font-size: 1.25rem;
  text-decoration: none;
  color: #ffffff;
}
</style>
