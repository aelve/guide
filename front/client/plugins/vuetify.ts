import Vue from 'vue'
import Vuetify from 'vuetify/lib'
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome'
import icons from 'client/icons'

const vuetifyIcons = {}
const addIcon = ({ prefix, iconName }) => {
  vuetifyIcons[iconName] = {
    component: 'font-awesome-icon',
    props: {
      icon: [prefix, iconName]
    }
  }
}
icons.forEach(addIcon)

Vue.component('font-awesome-icon', FontAwesomeIcon)
Vue.use(Vuetify)

export default new Vuetify({
  icons: {
    values: vuetifyIcons
  }
})
