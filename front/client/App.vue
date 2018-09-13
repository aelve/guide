<template>
  <v-app>
    <toolbar></toolbar>
    <v-content>
      <v-container fluid>
        <router-view></router-view>
      </v-container>
    </v-content>
    <v-footer app></v-footer>
  </v-app>
</template>

<script lang="ts">
import Component from 'vue-class-component'
import Vue from 'vue'

import Toolbar from './components/Toolbar.vue'

@Component({
  components: {
    Toolbar
  }
})
export default class RootComponent extends Vue {}
</script>

<style>
*,
*:before,
*:after {
  box-sizing: border-box;
}
</style>