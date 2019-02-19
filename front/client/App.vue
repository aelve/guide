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
import { Vue, Component, Watch } from 'vue-property-decorator'
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
  margin: 0;
}
code {
  color: #000;
  font-weight: 500; 
  box-shadow: none;
}
pre code {
  background-color: #f5f5f5;
  color: #bd4147;
  font-weight: 900;
  box-shadow: 0px 2px 1px -1px rgba(0, 0, 0, 0.2),
    0px 1px 1px 0px rgba(0, 0, 0, 0.14), 0px 1px 3px 0px rgba(0, 0, 0, 0.12);
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
</style>
