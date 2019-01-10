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
          <v-btn
            flat
            icon
            title="move item down"
          >
            <v-icon>fas fa-arrow-up</v-icon>
          </v-btn>

          <v-btn
            flat
            icon
            title="move item down"
          >
            <v-icon>fas fa-arrow-down</v-icon>
          </v-btn>

          <v-btn
            flat
            icon
            title="edit item info"
            @click="toggleEditItemInfoMenu"
          >
            <v-icon>fas fa-cog</v-icon>
            <v-icon
              v-if="isItemInfoEdited"
              class="edit-item-info-changed-icon"
              color="#6495ed"
              size="8"
            >
              fas fa-circle
            </v-icon>
          </v-btn>

          <v-btn
            flat
            icon
            title="delete item"
            @click="openConfirmDeleteDialog"
          >
            <v-icon>fas fa-times</v-icon>
          </v-btn>
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
            @click="saveItemInfo"
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
import ConfirmDialog from 'client/components/ConfirmDialog.vue'

@Component({
  components: {
    ConfirmDialog
  }
})
export default class CategoryItemToolbar extends Vue {
  @Prop(String) itemUid!: string
  @Prop(String) itemName!: string
  @Prop(String) itemLink!: string
  @Prop(String) itemGroup!: string
  @Prop(Object) itemHackage!: string

  isEditItemInfoMenuOpen: boolean = false
  isDeleteItemDialogOpen: boolean = false
  itemNameEdit: string = this.itemName
  itemLinkEdit: string = this.itemLink

  @Watch('itemName')
  onItemNameChange (newVal: string) {
    this.itemNameEdit = newVal
  }

  @Watch('itemLink')
  onItemLinkChange (newVal: string) {
    this.itemLinkEdit = newVal
  }

  get isItemInfoEdited () {
    return this.itemName !== this.itemNameEdit || this.itemLink !== this.itemLinkEdit
  }

  get isInfoSaveEnabled () {
    return this.isItemInfoEdited && this.itemNameEdit
  }

  toggleEditItemInfoMenu () {
    this.isEditItemInfoMenuOpen = !this.isEditItemInfoMenuOpen
  }

  openConfirmDeleteDialog () {
    this.isDeleteItemDialogOpen = true
  }

  async saveItemInfo (): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemInfo', {
      id: this.itemUid,
      body: {
        name: this.itemNameEdit,
        link: this.itemLinkEdit.trim() || null
      }
    })
    await this.$store.dispatch('category/reloadCategory')
    this.toggleEditItemInfoMenu()
  }

  async deleteItem (): Promise<void> {
    await this.$store.dispatch('categoryItem/deleteItemById', this.itemUid)
    await this.$store.dispatch('category/reloadCategory')
  }
}
</script>

<style scoped>
.edit-item-info-changed-icon {
  position: absolute;
  bottom: 0;
  right: 0;
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
