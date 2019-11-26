<template>
  <header class="app-toolbar">
    <div class="app-toolbar__items content-centered">
      <Logo :class="{ 'mobile-hidden': !isSearchBarHidden }"/>

      <SearchBar
        class="app-toolbar__search-bar"
        :class="{ 'mobile-hidden': isSearchBarHidden }"
        ref="searchBar"
      />
      <v-btn
        text
        icon
        small
        aria-label="Search"
        v-tooltip="'Search'"
        color="#fff"
        class="mobile-displayed ml-2"
        @click="toggleSearchBar"
      >
        <v-icon size="1rem">$vuetify.icons.{{isSearchBarHidden ? 'search' : 'times'}}</v-icon>
      </v-btn>
    </div>
  </header>
</template>

<script lang="ts">
import Vue from 'vue'
import SearchBar from 'client/components/SearchBar.vue'
import Logo from 'client/components/Logo.vue'
import Component from 'vue-class-component'

@Component({
  components: {
    SearchBar,
    Logo
  }
})
export default class Toolbar extends Vue {
  isSearchBarHidden: boolean = true

  toggleSearchBar () {
    this.isSearchBarHidden = !this.isSearchBarHidden
    if (!this.isSearchBarHidden) {
      this.$nextTick(() => this.$refs.searchBar.focus())
    }
  }
}
</script>

<style lang="postcss" scoped>
.app-toolbar {
  position: fixed;
  top: 0;
  left: 0;
  right: 0;
  background: #232323;
  height: 64px;
  z-index: 5;
}
.app-toolbar__items {
  display: flex;
  justify-content: space-between;
  align-items: center;
  height: 100%;
}
.mobile-displayed {
  display: none;
}
@media screen and (max-width: 475px) {
  .app-toolbar__search-bar {
    width: 100%;
    max-width: unset;
  }
  .mobile-hidden {
    display: none;
  }
  .mobile-displayed {
    display: inline-flex;
  }
}
</style>
