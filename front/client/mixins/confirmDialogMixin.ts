import Vue from 'vue'
import { Mixin } from 'vue-mixin-decorator'
import ConfirmDialog from 'client/components/ConfirmDialog.vue'
import DeferredPromise from 'utils/DeferredPromise'

const ComponentClass = Vue.extend(ConfirmDialog)

interface IConfirmDialogProps {
  fullText: string
  text: string
  confirmBtnText?: string
  cancelBtnText?: string
}

@Mixin
export default class ConfirmDialogMixin extends Vue {
  async _confirm ({ text, fullText, confirmBtnText, cancelBtnText }: IConfirmDialogProps): Promise<boolean> {
    const instance = new ComponentClass({
      propsData: {
        value: true,
        text,
        fullText,
        confirmBtnText,
        cancelBtnText
      }
    })
    instance.$mount()
    const deferredPromise = new DeferredPromise()
    this.$el.appendChild(instance.$el)
    instance.$on('confirmed', () => {
      instance.$destroy()
      deferredPromise.resolve(true)
    })
    instance.$on('canceled', () => {
      instance.$destroy()
      deferredPromise.resolve(false)
    })
    return deferredPromise
  }
}
