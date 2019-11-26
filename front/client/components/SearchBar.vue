<template>
  <v-form
    class="search-bar"
    @keydown.enter.native.prevent="processSearchQuery"
  >
    <v-text-field
      solo
      dark
      hide-details
      label="Search in all pages"
      class="search-bar__input"
      ref="input"
      v-model="searchQuery"
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
        v-show="searchQuery"
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
  searchQuery = this.$store.state.wiki.searchQuery

  processSearchQuery () {
    const searchResultsRouteName = 'SearchResults'
    const isSearchCurrentRoute = this.$route.name === searchResultsRouteName
    // same commit statement called in router 'SearchResults' route's "beforeEnter", but if its same route its not called
    // searchQuery is watched used in "SearchResults" component
    if (isSearchCurrentRoute) {
      this.$store.commit('wiki/setSearchQuery', this.searchQuery)
    }
    this.$router.push({ name: searchResultsRouteName, query: { query: this.searchQuery } })
  }

  get inputIconsColor () {
    return this.isFocused ? '#717171' : 'rgba(255, 255, 255, 0.7)'
  }

  clear () {
    this.searchQuery = ''
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
