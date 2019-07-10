<template>
  <v-app>
    <toolbar />
    <v-content>
      <router-view />
    </v-content>
    <a-footer />
  </v-app>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Watch } from 'vue-property-decorator'
import AFooter from 'client/components/AFooter.vue'
import Toolbar from 'client/components/Toolbar.vue'
import * as nprogress from 'nprogress'
import 'nprogress/nprogress.css'

nprogress.configure({ showSpinner: false })

@Component({
  components: {
    Toolbar,
    AFooter
  }
})
export default class RootComponent extends Vue {
  beforeMount () {
    // This package can only be loaded after mounted (on client only) cause it uses "document"
    // it is used in MarkdownEditor.vue and to make it work faster in that component we preload it here
    import('easymde')
  }

  get isPageLoading () {
    return this.$store.state.isPageLoading
  }

  @Watch('isPageLoading')
  toogleLoading (isPageLoading: boolean) {
    isPageLoading ? nprogress.start() : nprogress.done()
  }
}
</script>

<style>
*,
*:before,
*:after {
  box-sizing: border-box;
}
p {
  margin-bottom: 10px;
}
li p {
  margin-bottom: 0;
}
ul li:not(:last-child) {
  margin-bottom: 2px;
}
code {
  color: inherit;
  text-decoration: inherit;
  padding: 2px 4px;
  font-weight: 500;
  box-shadow: none;
  background: rgba(10, 10, 10, 0.04);
}
/* Vuetify css adds unwanted space to start and end of code and kbd tag */
code:after,
kbd:after,
code:before,
kbd:before {
  content: "";
  letter-spacing: initial;
}
pre code {
  background-color: rgba(10, 10, 10, 0.04);
  color: #bd4147;
  font-weight: 900;
  border-radius: 5px;
  border-width: 1px;
  border-style: solid;
  border-color: rgba(10, 10, 10, 0.05);
}
code.sourceCode {
  min-width: 100%;
  padding: 8px;
}
.sourceCode:not(:last-child) code.sourceCode {
  margin: 0 0 5px;
}
a {
  text-decoration-line: none;
  color: #0061c0;
}
a:hover {
  text-decoration-line: underline;
}
.text-transform-none {
  text-transform: none !important;
}
.position-relative {
  position: relative;
}
div.sourceCode {
  overflow-x: auto;
}
table.sourceCode,
tr.sourceCode,
td.lineNumbers,
td.sourceCode {
  margin: 0;
  padding: 0;
  vertical-align: baseline;
  border: none;
}
table.sourceCode {
  width: 100%;
  line-height: 100%;
}
td.lineNumbers {
  text-align: right;
  padding-right: 4px;
  padding-left: 4px;
  color: #aaaaaa;
  border-right: 1px solid #aaaaaa;
}
td.sourceCode {
  padding-left: 5px;
}
.v-input--has-state.error--text .v-label {
  animation: none;
}
blockquote {
  background: rgba(10, 10, 10, 0.075);
  border-left: 5px solid rgba(10, 10, 10, 0.15);
  padding: 1px 1em;
  margin-left: 0;
  margin-right: 0;
  border-radius: 5px;
}
/* vuetify v-select component when opened almost overlays its title */
.v-menu__content {
  margin-top: 5px;
}
</style>
