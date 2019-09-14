import Vue from 'vue'
import { Mixin } from 'vue-mixin-decorator'
import ErrorToast from 'client/components/ErrorToast.vue'

const ComponentClass = Vue.extend(ErrorToast)

@Mixin
export default class ErrorToastMixin extends Vue {
  async errorToast (options) {
    let message
    let details
    if (typeof options === 'string') {
      message = options
      details = null
    } else if (typeof options === 'object') {
      message = options.message
      details = options.details
    } else {
      throw new Error('errorToast method called with wrong argument type')
    }

    const instance = new ComponentClass({
      propsData: {
        message,
        details
      },
      parent: this
    })
    instance.$mount()
    instance.$on('emit', () => {
      // wait for animation of disappear to play
      setTimeout(() => instance.$destroy(), 300)
    })
    this.$el.appendChild(instance.$el)
  }
}
