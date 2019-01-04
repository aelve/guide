<template>
  <v-app>
    <toolbar />
    <v-content>
      <router-view></router-view>
    </v-content>
    <a-footer />
  </v-app>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import AFooter from 'client/components/AFooter.vue'

import Toolbar from './components/Toolbar.vue'

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
code.sourceCode {
  min-width: 100%;
  padding: 8px;
  margin: 0 0 15px;
}
/* Transition styles */
/* .slidedown-enter-active {
  transition: transform .3s cubic-bezier(0.165, 0.84, 0.44, 1);
}

.slidedown-leave-active {
  transition: transform .3s cubic-bezier(0.895, 0.03, 0.685, 0.22);
}

.slidedown-enter, .slidedown-leave-to {
  transform: scaleY(0);
} */
a {
  text-decoration-line: none;
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
