<template>
  <v-app>
    <toolbar />
    <v-content class="app-content">
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

<style lang="postcss">
*,
*:before,
*:after {
  box-sizing: border-box;
}
body, .v-application {
  font-family: -apple-system, system-ui, BlinkMacSystemFont, "Segoe UI", Roboto,
    "Helvetica Neue", Arial, sans-serif;
}

html {
  /*
    Formula is: calc(
      [min-font-size] +
      ([max-font-size] - [min-font-size] * (100vw - [min-vw]) / ([max-vw] - [min-vw])
    ); 
  */
  font-size: calc(16px + (18 - 16) * (100vw - 320px) / (1920 - 320));
}
:root {
  --h1-font-size: 1.8rem;
  --h2-font-size: 1.4rem;
  --h3-font-size: 1.17rem;
  --h4-font-size: 1rem;
  --h5-font-size: 0.83rem;
  --h6-font-size: 0.75rem;
}

h1 {
  font-size: var(--h1-font-size);
  margin: 1.95rem 0 0.6rem;
}
h2 {
  font-size: var(--h2-font-size);
  margin: 1.65rem 0 0.45rem;
}
h3 {
  font-size: var(--h3-font-size);
  margin: 1.35rem 0 0.35rem;
}
h4 {
  font-size: var(--h4-font-size);
  margin: 1rem 0 0.25rem;
}
h5 {
  font-size: var(--h5-font-size);
  margin: 0.8rem 0 0.2rem;
}
h6 {
  font-size: var(--h6-font-size);
  margin: 0.5rem 0 0;
}

.text-h1 {
  font-size: var(--h1-font-size);
}
.text-h2 {
  font-size: var(--h2-font-size);
}
.text-h3 {
  font-size: var(--h3-font-size);
}
.text-h4 {
  font-size: var(--h4-font-size);
}
.text-h5 {
  font-size: var(--h5-font-size);
}
.text-h6 {
  font-size: var(--h6-font-size);
}

p {
  margin: 0.2rem 0 1.2rem 0;
}

ul,
ol {
  margin: 0.8rem 0;
}

ul > li:not(:last-child),
ol > li:not(:last-child) {
  margin-bottom: 5px;
}

li p {
  margin-top: 0;
}

code {
  color: inherit;
  text-decoration: inherit;
  padding: 2px 4px;
  font-weight: 500;
  box-shadow: none;
  background: rgba(10, 10, 10, 0.04);

  &:not(.sourceCode) {
    font-weight: 800;
    line-height: 125%;
    padding: 2px 3px;
    border: 1px solid rgba(29, 28, 29, 0.13);
    border-radius: 3px;
    background-color: #fafafa;
    box-shadow: none;
    font-weight: normal;
  }
}
/* Vuetify css adds unwanted space to start and end of code and kbd tag */
.v-application code:after,
.v-application kbd:after,
.v-application code:before,
.v-application kbd:before {
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
  margin: 0 0 0.2rem;
}
a,
/* So much selectors is required to overwrite vuetify styles */
.v-application .app-content a {
  text-decoration-line: none;
  color: #005ebd;

  &:hover {
    text-decoration-line: underline;
  }
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
  padding: 0.4em 1em;
  margin-left: 0;
  margin-right: 0;
  border-radius: 5px;

  > *:first-child {
    margin-top: 0;
  }
  > *:last-child {
    margin-bottom: 0;
  }
}
.svg-inline--fa {
  width: 1.25em !important;
  text-align: center !important;
}
.v-toolbar__title {
  letter-spacing: inherit;
}
/* Some useless and obstructive div that vuetify add along with "v-menu" component */
.v-menu--inline {
  display: none !important;
}
</style>

<style lang="postcss" scoped>
/* Should be same padding on any screen cause it changes with toolbar and we fixed toolbar's height (see Toolbar.vue) height on each screen  */
.app-content {
  padding: 100px 0px 36px !important;
  line-height: 150%;
}
.app-content >>> > .v-content__wrap > .container {
  padding: 12px;
}
</style>
