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
        <a
          target="_blank"
          href="https://aelve.com/"
        >
          Aelve
        </a>
      </span>
      <span>
        <a
          class="footer-link"
          target="_blank"
          href="https://github.com/aelve/guide"
        >
          source
        </a>
        on GitHub
      </span>
      <span>
        licensed under
        <a
          target="_blank"
          href="https://creativecommons.org/licenses/by-sa/4.0/"
        >
          CC BY-SA 4.0
        </a>
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
