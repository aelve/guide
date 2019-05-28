import Vue from 'vue'
import VueRouter from 'vue-router'
import Vuex from 'vuex'
import Vuetify from 'vuetify'
import { sync } from 'vuex-router-sync'
import ALink from 'client/components/ALink.vue'
import confirmDialogMixin from 'client/mixins/confirmDialogMixin'
import 'vuetify/dist/vuetify.css'
import { library } from '@fortawesome/fontawesome-svg-core'
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'
import { fas } from '@fortawesome/free-solid-svg-icons'
library.add(fas)

import 'client/assets/code-highlight.css'

import AppComponent from './App.vue'
import { createRouter } from './router'
import { createStore } from './store'

const icons = {}
// TODO import and add only used icons for production
Object.values(fas).forEach(({ iconName }) => {
  icons[iconName] = {
    component: 'font-awesome-icon',
    props: {
      icon: iconName
    }
  }
})

function initVue () {
  Vue.use(VueRouter)
  Vue.use(Vuex)
  Vue.mixin(confirmDialogMixin)
  Vue.component('ALink', ALink)
  Vue.component('font-awesome-icon', FontAwesomeIcon)
  Vue.use(Vuetify, {
    iconfont: 'faSvg',
    icons
  })
}

function createApp () {
  const router = createRouter()
  const store = createStore()

  sync(store, router)

  const app = new Vue({
    router,
    store,
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
