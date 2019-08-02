<template>
  <v-app-bar
    dark
    app
    color="#232323"
  >
    <v-toolbar-title>
      <logo :class="{ 'mobile-hidden': !isSearchFieldHidden }"/>
    </v-toolbar-title>

    <v-spacer></v-spacer>

    <search-field
      :class="{ 'mobile-hidden': isSearchFieldHidden }"
      ref="searchField"
    />
    <v-btn
      text
      icon
      title="Search"
      color="#fff"
      class="mobile-displayed"
      @click="toggleSearchField"
    >
      <v-icon size="20">$vuetify.icons.search</v-icon>
    </v-btn>
  </v-app-bar>
</template>

<script lang="ts">
import Vue from 'vue'
import SearchField from 'client/components/SearchField.vue'
import Logo from 'client/components/Logo.vue'
import Component from 'vue-class-component'

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
.mobile-displayed {
  display: none;
}
@media screen and (max-width: 475px) {
  .mobile-hidden {
    display: none;
  }
  .mobile-displayed {
    display: inline-flex;
  }
}
</style>
