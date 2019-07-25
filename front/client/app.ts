import Vue from 'vue'
import VueRouter from 'vue-router'
import Vuex from 'vuex'
import vuetify from 'client/plugins/vuetify'
import { sync } from 'vuex-router-sync'
import ALink from 'client/components/ALink.vue'
import confirmDialogMixin from 'client/mixins/confirmDialogMixin'

import 'client/assets/code-highlight.css'

import AppComponent from './App.vue'
import { createRouter } from './router'
import { createStore } from './store'

function initVue () {
  Vue.use(VueRouter)
  Vue.use(Vuex)
  Vue.mixin(confirmDialogMixin)
  Vue.component('ALink', ALink)
}

function createApp () {
  const router = createRouter()
  const store = createStore()

  sync(store, router)

  const app = new Vue({
    router,
    store,
    vuetify,
    render: h => h(AppComponent)
  })

  return {
    app,
    router,
    store
  }
}

initVue()

export {
  createApp
}
