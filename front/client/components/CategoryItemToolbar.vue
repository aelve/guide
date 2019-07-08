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
        color="#dedede"
        class="elevation-2"
        @click.stop=""
      >
        <v-toolbar-title class="headline">
          <router-link
            :to="{hash:`item-${itemUid}`}"
            class="category-item-anchor"
          >#</router-link>&nbsp;<a-link
            v-if="itemLink"
            :url="itemLink"
            openInNewTab
          >{{ itemName }}</a-link>
          <span v-else>{{ itemName }}</span><template v-if="this.itemHackage">&nbsp;(<a
            target="_blank"
            :href="`https://hackage.haskell.org/package/${this.itemHackage}`"
          >Hackage</a>)</template>
        </v-toolbar-title>
        <v-spacer></v-spacer>
        <v-toolbar-items class="category-item-toolbar-btns">
          <category-item-btn
            title="Move item up"
            icon="arrow-up"
            @click="moveItem('up')"
          />

          <category-item-btn
            title="Move item down"
            icon="arrow-down"
            @click="moveItem('down')"
          />

          <category-item-btn
            title="Edit item info"
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
            title="Delete item"
            icon="trash-alt"
            @click="deleteItem"
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
            v-model="itemHackageEdit"
            label="Name on Hackage (optional)"
          />
          <v-text-field
            v-model="itemLinkEdit"
            label="Site (optional)"
          />
        </v-flex>
        <v-flex align-self-end>
          <v-btn
            class="mr-0"
            title="Save"
            :disabled="!isInfoSaveEnabled"
            @click="updateItemInfo"
          >
            Save
          </v-btn>
        </v-flex>
      </v-layout>
    </v-expansion-panel-content>
  </v-expansion-panel>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import normalizeUrl from 'normalize-url'
import Confirm from 'client/helpers/ConfirmDecorator'
import CategoryItemBtn from 'client/components/CategoryItemBtn.vue'

@Component({
  components: {
    CategoryItemBtn
  }
})
export default class CategoryItemToolbar extends Vue {
  @Prop(String) itemUid!: string
  @Prop(String) itemName!: string
  @Prop(String) itemLink!: string
  @Prop(String) itemHackage!: string

  isEditItemInfoMenuOpen: boolean = false
  itemNameEdit: string = this.itemName
  itemLinkEdit: string = this.itemLink
  itemHackageEdit: string = this.itemHackage

  get isItemInfoEdited () {
    return this.itemName !== this.itemNameEdit || this.itemLink !== this.itemLinkEdit || this.itemHackage !== this.itemHackageEdit
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

  async updateItemInfo (): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemInfo', {
      id: this.itemUid,
      body: {
        name: this.itemNameEdit,
        link: this.getLinkForSave(),
        hackage: this.itemHackageEdit
      }
    })
    await this.$store.dispatch('category/reloadCategory')
    this.toggleEditItemInfoMenu()
  }

  getLinkForSave () {
    const trimmed = this.itemLinkEdit && this.itemLinkEdit.trim()
    if (!trimmed) {
      return null
    }
    return this.itemLink === trimmed ? this.itemLink : normalizeUrl(trimmed)
  }

  @Confirm({ text: 'delete this item' })
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
.category-item-anchor {
  color: rgb(151, 151, 151);
}
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
}
</style>
