<template>
  <v-toolbar dark app>
    <v-toolbar-title>
      <logo />
    </v-toolbar-title>
    <v-spacer></v-spacer>
    <search-field />
  </v-toolbar>
</template>

<script lang="ts">
import Vue from 'vue'
import SearchField from 'client/components/Search.vue'
import Logo from 'client/components/Logo.vue'
import Component from 'vue-class-component'
import axios from 'axios'

@Component({
  components: {
    SearchField,
    Logo
  }
})
export default class Toolbar extends Vue {}
</script>

<style scoped>
>>> .v-toolbar__content {
  height: 64px !important;
}
</style>
