<template>
  <v-btn
    text
    class="category-header-btn"
    color="grey darken-2"
    :aria-label="text"
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
/* .v-btn selector added for more specificity to override vuetify styles without using !important*/
.v-btn.category-header-btn {
  padding: 0 4px;
  font-size: 0.8rem;
  text-transform: none;
}
</style>
