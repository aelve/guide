<template>
  <v-toolbar dark>
    <v-toolbar-side-icon></v-toolbar-side-icon>
    <v-toolbar-title>
      <logo />
    </v-toolbar-title>
    <v-spacer></v-spacer>
    <search-field />
    <v-toolbar-items class="hidden-sm-and-down">
      <v-btn
        flat
        v-on:click="getApi"
      >
        Login
      </v-btn>
    </v-toolbar-items>
  </v-toolbar>
</template>

<script lang="ts">
import Vue from 'vue';
import SearchField from 'client/components/Search.vue';
import Logo from 'client/components/Logo.vue';
import Component from 'vue-class-component';
import axios from "axios";

@Component({
  components: {
    SearchField,
    Logo
  }
})
export default class Toolbar extends Vue {
  async getApi() {
    const { data } = await axios.get('api');
  }
}
</script>
