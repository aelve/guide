<template>
  <div class="category-item-traits">
    <category-item-trait
      type="pro"
      :itemId="itemId"
      :traits="pros"
    />
    <category-item-trait
      type="con"
      :itemId="itemId"
      :traits="cons"
    />
  </div>
</template>

<script lang="ts">
import { Vue, Component, Prop } from 'vue-property-decorator'
import CategoryItemTrait from 'client/components/CategoryItemTrait.vue'

@Component({
  components: {
    CategoryItemTrait
  }
})
export default class CategoryItemTraits extends Vue {
  @Prop(Array) pros!: any[]
  @Prop(Array) cons!: any[]
  @Prop(String) itemId!: string

  isProsEdit: boolean = false
  isConsEdit: boolean = false

  toggleProsEdit () {
    this.isProsEdit = !this.isProsEdit
  }

}
</script>

<style scoped>
.category-item-traits {
  display: flex;
}

.category-item-traits > * {
  flex: 1;
}

.category-item-traits > *:not(:last-child) {
  margin-right: 20px;
}
</style>