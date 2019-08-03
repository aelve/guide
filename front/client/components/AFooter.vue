<template>
  <v-footer
    class="pa-2"
    height="auto"
    color="lighten-1"
  >
    <v-layout
      justify-center
      align-center
      row
      wrap
      class="footer"
    >
      <span>
        made by
        <a-link
          openInNewTab
          url="https://aelve.com/"
        >
          Aelve
        </a-link>
      </span>
      <span>
        <a-link
          openInNewTab
          url="https://github.com/aelve/guide"
          class="footer-link"
        >
          source
        </a-link>
        on GitHub
      </span>
      <span>
        licensed under
        <a-link
          openInNewTab
          url="http://guide.aelve.com:4801/license"
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

<style lang="postcss" scoped>
.footer-link {
  display: inline-block;
}
.footer {
  margin: 0 -10px 0 0;

  > * {
    margin-right: 10px;
  }
}
</style>
