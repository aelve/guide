<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Watch } from 'vue-property-decorator'

function getComponentAndItsChildren (component, result?) {
  if (!result) {
    result = []
  }
  if (!result.includes(component)) {
    result.push(component)
  }
  const children = component.$children
  children.forEach(x => getComponentAndItsChildren(x, result))

  return result
}

/**
 * When this component slot (supports only one slot) changes it prerenders new slot in memory
 * and waits for slot (and its' children) "serverPrefetch" execution before rendering it
 */
@Component
export default class ClientServerPrefetch extends Vue {
  // vNodeToRender is not set here (as reactive property) to prevent automatic "render" function call when it changes
  isServerPrefetchProcessing = false

  @Watch('isServerPrefetchProcessing')
  onServerPrefetch (val) {
    this.$store.commit('togglePageLoading')
  }

  onServerPrefetchFinish () {
    const previousVNode = this.vNodeToRender
    this.vNodeToRender = this.$slots.default[0]
    this.$forceUpdate()
    this.isServerPrefetchProcessing = false
    previousVNode.data.keepAlive = false
    previousVNode.componentInstance.$destroy()
  }

  async preloadSlotComponent () {
    this.isServerPrefetchProcessing = true
    try {
      const slot = this.$slots.default[0]
      // This is vue's internal function that inits component
      this._update(slot, false)
      const componentInstance = slot.componentInstance
      const components = getComponentAndItsChildren(componentInstance)
      const getServerPrefetchFunc = x => x.$options.serverPrefetch && x.$options.serverPrefetch[0]
      const componentsWithServerPrefetch = components.filter(getServerPrefetchFunc)
      await Promise.all(componentsWithServerPrefetch.map(x => getServerPrefetchFunc(x).call(x)))
      this.onServerPrefetchFinish()
    } catch (e) {
      this.isServerPrefetchProcessing = false
      this.$router.back()
    }
  }

  render (h) {
    const slot = this.$slots.default[0]
    // If we don't set keepAlive to true, component will be automatically destroyed when slot changes
    // and also new slot will be rendered twice: 1 - in preloadSlotComponent func, 2 - when return here
    slot.data.keepAlive = true

    if (!this.vNodeToRender || slot === this.vNodeToRender || slot.tag === this.vNodeToRender.tag) {
      this.vNodeToRender = slot
    } else {
      // if slot was changed we dont render it immediatly but firstly execute its (and its children) 'serverPrefetch' functions
      this.preloadSlotComponent()
    }

    return this.vNodeToRender
  }
}
</script>