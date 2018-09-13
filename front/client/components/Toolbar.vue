<template>
  <v-toolbar dark>
    <v-toolbar-side-icon></v-toolbar-side-icon>
    <v-toolbar-title>Aelve Guide</v-toolbar-title>
    <v-spacer></v-spacer>
    <search></search>
    <v-toolbar-items class="hidden-sm-and-down">
      <v-btn flat>Login</v-btn>
    </v-toolbar-items>
  </v-toolbar>
</template>

<script lang="ts">
import Vue from 'vue';
import Search from './Search.vue';
import Component from 'vue-class-component';

@Component({
  components: {
    Search
  }
})
export default class Toolbar extends Vue {}
</script>
