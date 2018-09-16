<template>
  <v-app>
    <toolbar></toolbar>
    <v-content>
      <v-container fluid>
        <router-view></router-view>
      </v-container>
    </v-content>
    <a-footer />
  </v-app>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import AFooter from 'client/components/AFooter.vue'

import Toolbar from './components/Toolbar.vue'

@Component({
  components: {
    Toolbar,
    AFooter
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