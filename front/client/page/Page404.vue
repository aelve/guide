<template>
  <div class="page404 pt-5">
    <h1 class="display-4 page404__number-header font-weight-bold">404</h1>
    <h2 class="display-2 mt-4">
      Page Not Found
    </h2>
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import { Component } from 'vue-property-decorator'

@Component
export default class Page404 extends Vue { }
</script>

<style lang="postcss" scoped>
.page404 {
  text-align: center;
  display: flex;
  height: 100%;
  flex: 1;
  flex-direction: column;
  align-items: center;

  &__number-header {
    letter-spacing: 1.2rem !important;
  }
}
</style>

