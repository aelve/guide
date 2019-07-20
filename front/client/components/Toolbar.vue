<template>
  <v-toolbar dark app>
    <v-toolbar-title>
      <logo />
    </v-toolbar-title>
    <v-spacer></v-spacer>
    <search-field
      :class="{ 'mobile-hidden': isSearchFieldHidden }"
      ref="searchField"
    />
    <v-btn
      flat
      icon
      title="Search"
      color="#fff"
      class="mobile-displayed"
      @click="toggleSearchField"
    >
      <v-icon size="20">$vuetify.icons.search</v-icon>
    </v-btn>
  </v-toolbar>
</template>

<script lang="ts">
import Vue from 'vue'
import SearchField from 'client/components/SearchField.vue'
import Logo from 'client/components/Logo.vue'
import Component from 'vue-class-component'
import axios from 'axios'

@Component({
  components: {
    SearchField,
    Logo
  }
})
export default class Toolbar extends Vue {
  isSearchFieldHidden: boolean = true

  toggleSearchField () {
    this.isSearchFieldHidden = !this.isSearchFieldHidden
    if (!this.isSearchFieldHidden) {
      this.$nextTick(() => this.$refs.searchField.focus())
    }
  }
}
</script>

<style lang="postcss" scoped>
>>> .v-toolbar__content {
  height: 64px !important;
}
.mobile-hidden {
  display: block;
}
.mobile-displayed {
  display: none;
}
@media screen and (max-width: 475px) {
  .mobile-hidden {
    display: none;
  }
  .mobile-displayed {
    display: block;
  }
}
</style>
