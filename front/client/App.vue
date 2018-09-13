<template>
  <v-app>
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

@Component({
  components: {
    AFooter
  }
})
export default class RootComponent extends Vue { }
</script>

<style>
*,
*:before,
*:after {
  box-sizing: border-box;
}
</style>