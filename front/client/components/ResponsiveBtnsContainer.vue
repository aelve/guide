<script lang="tsx">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import { VMenu, VList, VListItem, VBtn, VIcon } from 'vuetify/lib'
import CategoryHeaderBtn from 'client/components/CategoryHeaderBtn.vue'

@Component
export default class ResponsiveMenu extends Vue {
  @Prop() menuAttach

  render (h) {
    const menuItemsSlot = this.$slots.menuItems || this.$slots.default
    const menuItems = menuItemsSlot
      .map(slotItem => (
        <VListItem>
          {slotItem}
        </VListItem>
      ))

    const VMenuScopedSlots = {
      activator: ({ on }) => (
        <span class='responsive-bar__mobile-menu-btn-wrap'>
          {this.$scopedSlots.menuBtn({ on })}
        </span>
      )
    }
    return (
      <div>
        <div class='responsive-bar__desktop-wrap'>
          {this.$slots.default}
        </div>

        <VMenu
          bottom
          left
          offset-y
          attach={this.menuAttach}
          scopedSlots={VMenuScopedSlots}
        >
          <VList class='responsive-bar__mobile-menu-list'>
            {menuItems}
          </VList>
        </VMenu>
      </div>
    )
  }
}
</script>

<style lang="postcss" scoped>
.responsive-bar__mobile-menu-btn-wrap {
  display: none;
}
.responsive-bar__mobile-menu-list {
  >>> .v-list-item {
    height: 36px;
    padding: 0;
  }

  >>> button {
    display: flex;
    flex: 1 0 auto;
    height: 100% !important;
    min-width: 100% !important;
    margin: 0;
    padding: 0 6px;

    .v-btn__content {
      justify-content: flex-start;
    }
  }
}
@media (max-width: 768px) {
  .responsive-bar__mobile-menu-btn-wrap {
    display: block;
  }
  .responsive-bar__desktop-wrap {
    display: none !important;
  }
}
</style>