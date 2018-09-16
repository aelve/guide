<template>
  <v-toolbar dark>
    <v-toolbar-side-icon></v-toolbar-side-icon>
    <v-toolbar-title>Aelve Guide</v-toolbar-title>
    <v-spacer></v-spacer>
    <!-- <search></search> -->
    <v-toolbar-items class="hidden-sm-and-down">
      <v-btn v-on:click="getApi" flat>Login</v-btn>
    </v-toolbar-items>
  </v-toolbar>
</template>

<script lang="ts">
import Vue from 'vue';
import Search from './Search.vue';
import Component from 'vue-class-component';
import axios from "axios";

@Component({
  components: {
    Search
  }
})
export default class Toolbar extends Vue {
  async getApi() {
    const { data } = await axios.get('api');
    console.log(data);
  }
}
</script>
