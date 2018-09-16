<template>
  <v-footer
    height="auto"
    color="lighten-1"
  >
    <v-layout
      justify-center
      align-center
      row
      wrap
    >
      <span class="mr-3">
        made by 
        <a-link
          openInNewTab
          url="https://artyom.me/"
        >
          Artyom
        </a-link>
      </span>
      <span class="mr-3">
        <a-link
          openInNewTab
          url="https://github.com/aelve/guide"
        >
          source
        </a-link>
        /
        <a-link
          openInNewTab
          url="https://github.com/aelve/guide/issues"
        >
          issue tracker
        </a-link>
      </span>
      <span>
        licensed under
        <a-link
          openInNewTab
          url="http://aelve.com:4801/license"
        >
          CC+ BY-SA 4.0
        </a-link>
      </span>
    </v-layout>
  </v-footer>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'

@Component
export default class AFooter extends Vue { }
</script>
