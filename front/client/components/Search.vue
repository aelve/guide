<template>
  <v-text-field
    dark
    solo
    class="toollbar-search"
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
.toollbar-search {
  margin-right: 15px;
}

.toollbar-search >>> div {
  margin-bottom: 0 !important;
}
.toollbar-search >>> label {
  font-size: 18px;
}
</style>
