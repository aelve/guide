import Vue from 'vue'
import Vuetify from 'vuetify/lib'

import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'
import { library } from '@fortawesome/fontawesome-svg-core'
import { fas } from '@fortawesome/free-solid-svg-icons'
import { faSquare } from '@fortawesome/free-regular-svg-icons'
library.add(fas, faSquare)

const icons = {}
// TODO import and add only used icons for production
Object.values({ ...fas, faSquare }).forEach(({ prefix, iconName }) => {
  icons[iconName] = {
    component: 'font-awesome-icon',
    props: {
      icon: [prefix, iconName]
    }
  }
})

Vue.component('font-awesome-icon', FontAwesomeIcon)
Vue.use(Vuetify)

export default new Vuetify({
  icons: {
    iconfont: 'faSvg',
    values: icons
  }
})
