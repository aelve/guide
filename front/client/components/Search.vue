<template>
  <v-text-field
    dark
    solo
    class="toolbar-search"
    label="Search"
    :value="searchInput"
    @input="setSearchInput"
    @keyup.enter.native="processSearchInput"
  />
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'

@Component
export default class SearchField extends Vue {

  processSearchInput () {
    this.$router.push({ name: 'SearchResults', query: { query: this.searchInput } })
  }

  get searchInput () {
    return this.$store.state.wiki.searchInput
  }

  setSearchInput (value: string) {
    this.$store.commit('wiki/setSearchInput', value)
  }
}
</script>

<style scoped>
.toolbar-search >>> div {
  margin-bottom: 0 !important;
}
.toolbar-search >>> label {
  font-size: 18px;
}
</style>
