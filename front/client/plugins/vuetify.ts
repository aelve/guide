import Vue from 'vue'
import Vuetify from 'vuetify/lib'

import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'
import { library } from '@fortawesome/fontawesome-svg-core'
import { fas } from '@fortawesome/free-solid-svg-icons'
import { faSquare, faCheckSquare } from '@fortawesome/free-regular-svg-icons'
library.add(fas, faSquare, faCheckSquare)

const icons = {}
const addIcon = ({ prefix, iconName }, iconUsageName = iconName) => {
  icons[iconUsageName] = {
    component: 'font-awesome-icon',
    props: {
      icon: [prefix, iconName]
    }
  }
}
// TODO import and add only used icons for production
Object.values({ ...fas, faSquare, faCheckSquare }).forEach(x => addIcon(x))

// This is for vuetify v-checkbox, v-select components, which uses by default icons with such names
addIcon(faCheckSquare, 'checkboxOn')
addIcon(faSquare, 'checkboxOff')
addIcon(fas.faCaretDown, 'dropdown')

Vue.component('font-awesome-icon', FontAwesomeIcon)
Vue.use(Vuetify)

export default new Vuetify({
  icons: {
    values: icons
  }
})
