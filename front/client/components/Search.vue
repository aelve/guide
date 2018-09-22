<template>
  <v-text-field v-model="searchInput" 
                class="toollbar-search" 
                label="Search" 
                @keyup.enter.native="processSearchInput"
                dark solo></v-text-field>
</template>

<script lang="ts">
import Vue from 'vue';
import Component from 'vue-class-component';

@Component
export default class SearchField extends Vue {
  searchInput:string = ''

  processSearchInput() {
    window.open('`http://aelve.com:4801/haskell/?q=${this.searchInput}`', '_blank')
    console.log(this.searchInput);
    
    return this.searchInput
  }
}
</script>

<style scoped>
.toollbar-search >>> div {
  margin-bottom: 0 !important;
}
</style>
