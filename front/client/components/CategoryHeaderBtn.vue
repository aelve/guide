<template>
  <v-btn
    flat
    class="category-header-btn"
    :title="text"
    color="grey darken-2"
    v-bind="$attrs"
    v-on="$listeners"
  >
    <v-icon size="18" class="mr-1">{{`$vuetify.icons.${icon}`}}</v-icon>
    {{ text }}
  </v-btn>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'

@Component
export default class CategoryHeaderBtn extends Vue {
  @Prop(String) icon: string
  @Prop(String) text: string
}
</script>

<style scoped>
.category-header-btn {
  margin: 0;
  padding: 0 4px;
  font-weight: bold;
}
</style>
