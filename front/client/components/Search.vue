<template>
  <v-text-field class="toollbar-search" label="Search" dark solo></v-text-field>
</template>

<script lang="ts">
import Vue from 'vue';
import Component from 'vue-class-component';

@Component
export default class SearchField extends Vue {}
</script>

<style>
.toollbar-search div {
  margin-bottom: 0 !important;
}
</style>
