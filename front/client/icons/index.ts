import { library } from '@fortawesome/fontawesome-svg-core'
import {
  faPlus,
  faCog,
  faTrashAlt,
  faArrowUp,
  faArrowDown,
  faEllipsisV,
  faRss,
  faBars,
  faLink,
  faCircle,
  faTimes,
  faSearch,
  faCaretDown,
  faPen,
  faExclamationTriangle,
} from '@fortawesome/free-solid-svg-icons'
import { faSquare, faCheckSquare } from '@fortawesome/free-regular-svg-icons'

// This is for vuetify v-checkbox, v-select components, which uses by default icons with such names
faCheckSquare.iconName = 'checkboxOn'
faSquare.iconName = 'checkboxOff'
faCaretDown.iconName = 'dropdown'

const icons = [
  faPlus,
  faCog,
  faTrashAlt,
  faArrowUp,
  faArrowDown,
  faEllipsisV,
  faRss,
  faBars,
  faLink,
  faCircle,
  faTimes,
  faSearch,
  faCaretDown,
  faPen,
  faExclamationTriangle,
  faSquare,
  faCheckSquare
]
library.add(...icons)

export default icons
