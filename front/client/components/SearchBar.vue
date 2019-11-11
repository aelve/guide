<template>
  <v-form
    class="search-bar"
    @keydown.enter.native.prevent="processSearchInput"
  >
    <v-text-field
      solo
      dark
      hide-details
      label="Search in all pages"
      class="search-bar__input"
      ref="input"
      v-model="searchInput"
      @focus="isFocused = true"
      @blur="isFocused= false"
    >
      <v-icon
        slot="prepend-inner"
        size="1rem"
        :color="inputIconsColor"
      >$vuetify.icons.search</v-icon>
      <v-icon
        slot="append"
        role="button"
        size="0.9rem"
        class="search-bar__input__clear-btn"
        :color="inputIconsColor"
        v-show="searchInput"
        @click="clear"
      >$vuetify.icons.times</v-icon>
    </v-text-field>
  </v-form>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'

@Component
export default class SearchBar extends Vue {

  isFocused = false

  processSearchInput () {
    this.$router.push({ name: 'SearchResults', query: { query: this.searchInput } })
  }

  // TODO investigate is using computed necessary here
  // TODO serever rendering doesnt render value in text field
  get searchInput () {
    return this.$store.state.wiki.searchInput
  }

  set searchInput (value) {
    this.$store.commit('wiki/setSearchInput', value)
  }

  get inputIconsColor () {
    return this.isFocused ? '#717171' : 'rgba(255, 255, 255, 0.7)'
  }

  clear () {
    this.searchInput = ''
  }

  focus () {
    this.$refs.input.focus()
  }
}
</script>

<style lang="postcss" scoped>
.search-bar {
  width: 300px;
  max-width: 300px;
}
.search-bar__input {
  >>> .v-input__control {
    min-height: 36px;
  }
}
.search-bar:focus-within {
  .search-bar__input {
    >>> {
      .v-input__slot {
        background: #fafbfc;
      }
      label {
        color: #717171;
      }
      input {
        color: #000;
        caret-color: #000;
      }
    }
  }
}
.search-bar__input__clear-btn {
  cursor: pointer;
}
@media screen and (min-width: 780px) {
  .search-bar__input__clear-btn {
    display: none;
  }
}
</style>
