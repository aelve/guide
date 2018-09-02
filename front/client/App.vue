<template>
  <v-app>
    <v-toolbar app></v-toolbar>
    <v-content>
      <v-container fluid>
        <router-view></router-view>
      </v-container>
    </v-content>
    <v-footer app></v-footer>
  </v-app>
</template>

<script lang="ts">
import { AppComponent, Component } from "@vert/core";
import axios from "axios";

@Component
export default class RootComponent extends AppComponent {
  async mounted() {
    const { data: categories } = await axios.get("api/categories");
    console.log(categories);
  }
}
</script>
