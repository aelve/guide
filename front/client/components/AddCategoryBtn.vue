<template>
  <v-btn
    text
    class="ma-0 mt-1 px-2"
    color="grey darken-2"
    aria-label="Add new category"
    data-testid="CreateCategoryBtn"
    v-on="$listeners"
  >
    <v-icon size="14" class="mr-1" left v-text="'$vuetify.icons.plus'"></v-icon>
    Add new category
  </v-btn>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'

@Component
export default class AddCategoryBtn extends Vue { }
</script>

<style>
</style>