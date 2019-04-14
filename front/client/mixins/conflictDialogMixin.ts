import Vue from 'vue'
import { Mixin } from 'vue-mixin-decorator'
import ConflictDialog from 'client/components/ConflictDialog.vue'
import DeferredPromise from 'utils/DeferredPromise'

const ComponentClass = Vue.extend(ConflictDialog)

interface IConflictDialogProps {
  serverModified: string,
  modified: string,
  merged: string
}

@Mixin
export default class ConflictDialogMixin extends Vue {
  async openConflictDialog ({ serverModified, modified, merged }: IConflictDialogProps): Promise<string> {
    const instance = new ComponentClass({
      propsData: {
        value: true,
        serverModified,
        modified,
        merged
      }
    })
    instance.$mount()
    const deferredPromise = new DeferredPromise()
    this.$el.appendChild(instance.$el)
    instance.$on('save', (newVal) => {
      instance.$destroy()
      deferredPromise.resolve(newVal)
    })

    return deferredPromise
  }
}
