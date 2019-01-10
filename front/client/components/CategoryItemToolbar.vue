<template>
  <v-expansion-panel class="category-item-toolbar">
    <v-expansion-panel-content
      hide-actions
      lazy
      :value="isEditItemInfoMenuOpen"
    >
      <v-toolbar
        flat
        slot="header"
        color="#d6d6d6"
        class="elevation-2"
        @click.stop=""
      >
        <v-toolbar-title class="headline">
          <a-link
            v-if="itemLink"
            :url="itemLink"
            openInNewTab
          >
            {{ itemName }}
          </a-link>
          <span v-else> {{ itemName }} </span>
        </v-toolbar-title>
        <v-spacer></v-spacer>
        <v-toolbar-items class="category-item-toolbar-btns">
          <category-item-btn
            title="move item up"
            icon="arrow-up"
            @click="moveItem('up')"
          />

          <category-item-btn
            title="move item down"
            icon="arrow-down"
            @click="moveItem('down')"
          />

          <category-item-btn
            title="edit item info"
            icon="cog"
            @click="toggleEditItemInfoMenu"
          >
            <v-icon
              v-if="isItemInfoEdited"
              class="edit-item-info-changed-icon"
              color="#6495ed"
              size="8"
            >$vuetify.icons.circle</v-icon>
          </category-item-btn>

          <category-item-btn
            title="delete item"
            icon="trash-alt"
            @click="openConfirmDeleteDialog"
          />
        </v-toolbar-items>
      </v-toolbar>
      <v-layout column class="pa-3">
        <v-flex>
          <v-text-field
            v-model="itemNameEdit"
            label="Name"
          />
          <v-text-field
            v-model="itemLinkEdit"
            label="Site (optional)"
          />
        </v-flex>
        <v-flex align-self-end>
          <v-btn
            class="mr-0"
            :disabled="!isInfoSaveEnabled"
            @click="updateItemInfo"
          >
            Save
          </v-btn>
        </v-flex>
      </v-layout>
    </v-expansion-panel-content>

    <confirm-dialog
      confirmationText="delete this item"
      v-model="isDeleteItemDialogOpen"
      @confirmed="deleteItem"
    />
  </v-expansion-panel>
</template>

<script lang="ts">
import { Vue, Component, Prop, Watch } from 'vue-property-decorator'
import normalizeUrl from 'normalize-url'
import ConfirmDialog from 'client/components/ConfirmDialog.vue'
import CategoryItemBtn from 'client/components/CategoryItemBtn.vue'

@Component({
  components: {
    ConfirmDialog,
    CategoryItemBtn
  }
})
export default class CategoryItemToolbar extends Vue {
  @Prop(String) itemUid!: string
  @Prop(String) itemName!: string
  @Prop(String) itemLink!: string
  @Prop(String) itemGroup!: string
  @Prop(String) itemHackage!: string

  isEditItemInfoMenuOpen: boolean = false
  isDeleteItemDialogOpen: boolean = false
  itemNameEdit: string = this.itemName
  itemLinkEdit: string = this.itemLink

  get isItemInfoEdited () {
    return this.itemName !== this.itemNameEdit || this.itemLink !== this.itemLinkEdit
  }

  get isInfoSaveEnabled () {
    return this.isItemInfoEdited && this.itemNameEdit
  }

  @Watch('itemName')
  onItemNameChange (newVal: string) {
    this.itemNameEdit = newVal
  }

  @Watch('itemLink')
  onItemLinkChange (newVal: string) {
    this.itemLinkEdit = newVal
  }

  toggleEditItemInfoMenu () {
    this.isEditItemInfoMenuOpen = !this.isEditItemInfoMenuOpen
  }

  openConfirmDeleteDialog () {
    this.isDeleteItemDialogOpen = true
  }

  async updateItemInfo (): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemInfo', {
      id: this.itemUid,
      body: {
        name: this.itemNameEdit,
        link: this.getLinkForSave()
      }
    })
    await this.$store.dispatch('category/reloadCategory')
    this.toggleEditItemInfoMenu()
  }

  async getLinkForSave () {
    const trimmed = this.itemLinkEdit.trim()
    if (!trimmed) {
      return null
    }
    return this.itemLink === trimmed ? this.itemLink : normalizeUrl(trimmed)
  }

  async deleteItem (): Promise<void> {
    await this.$store.dispatch('categoryItem/deleteItemById', this.itemUid)
    await this.$store.dispatch('category/reloadCategory')
  }

  async moveItem (direction: string) {
    await this.$store.dispatch('categoryItem/moveItem', {
      id: this.itemUid,
      direction
    })
    await this.$store.dispatch('category/reloadCategory')
  }
}
</script>

<style scoped>
.edit-item-info-changed-icon {
  position: absolute;
  bottom: 0;
  right: 5px;
}
.category-item-toolbar >>> .v-toolbar__title {
  overflow: visible;
}
.category-item-toolbar {
  display: flex;
  box-shadow: none;
}
.category-item-toolbar >>> .v-expansion-panel__header {
  padding: 0;
  align-items: center;
  cursor: unset;
}
.category-item-toolbar >>> .v-expansion-panel__body {
  background: #d6d6d6;
}
.category-item-toolbar-btns > * {
  margin: 0 2px;
  color: #979797;
}
</style>
