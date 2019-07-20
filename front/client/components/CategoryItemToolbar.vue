<template>
  <v-expansion-panel class="category-item-toolbar">
    <v-expansion-panel-content
      hide-actions
      :value="isEditItemInfoMenuOpen"
    >
      <v-toolbar
        flat
        slot="header"
        color="#dedede"
        class="elevation-2"
        @click.stop=""
      >
        <v-toolbar-title class="text-h2">
          <span class="category-item-toolbar-title">
            <router-link
              :to="{hash:`item-${itemUid}`}"
              class="category-item-anchor"
            >#</router-link>

            <div class="category-item-name-and-badges">
              <a-link
                v-if="itemLink"
                class="category-item-name"
                :url="itemLink"
                openInNewTab
              >{{ itemName }}</a-link>
              <span class="category-item-name" v-else>{{ itemName }}</span>

              <div class="category-item-badges">
                <a
                  v-if="this.itemHackage"
                  class="text-h6 hackage-link"
                  target="_blank"
                  :href="`https://hackage.haskell.org/package/${this.itemHackage}`"
                >
                  <v-icon
                    color="#fff"
                    class="mr-1"
                    size="12"
                  >$vuetify.icons.link</v-icon>hackage</a>
              </div>
            </div>
          </span>
        </v-toolbar-title>

        <v-spacer></v-spacer>

        <v-toolbar-items>
          <div class="category-item-toolbar-btns">
            <category-item-btn
              iconSize="18"
              title="Move item up"
              icon="arrow-up"
              @click="moveItem('up')"
            />
            <category-item-btn
              iconSize="18"
              title="Move item down"
              icon="arrow-down"
              @click="moveItem('down')"
            />
            <category-item-btn
              iconSize="18"
              title="Edit item info"
              icon="cog"
              @click="toggleEditItemInfoMenu"
            >
              <v-icon
                v-if="isItemInfoEdited"
                class="unsaved-changes-icon"
                color="#6495ed"
                size="8"
              >$vuetify.icons.circle</v-icon>
            </category-item-btn>

            <category-item-btn
              iconSize="18"
              title="Delete item"
              icon="trash-alt"
              @click="deleteItem"
            />
          </div>

          <v-menu bottom left offset-y>
            <template v-slot:activator="{ on }">
              <v-btn
                flat
                icon
                title="Actions"
                class="category-toolbar-mobile-menu-btn"
                v-on="on"
              >
                <v-icon
                  size="18"
                  color="grey darken-2"
                >$vuetify.icons.bars</v-icon>
              </v-btn>
            </template>

            <v-list class="category-item-toolbar-mobile-menu-list">
              <v-list-tile>
                <category-item-btn
                  showTitle
                  iconSize="18"
                  title="Move item up"
                  icon="arrow-up"
                  @click="moveItem('up')"
                />
              </v-list-tile>
              <v-list-tile>
                <category-item-btn
                  showTitle
                  iconSize="18"
                  title="Move item down"
                  icon="arrow-down"
                  @click="moveItem('down')"
                />
              </v-list-tile>
              <v-list-tile>
                <category-item-btn
                  showTitle
                  iconSize="18"
                  title="Edit item info"
                  icon="cog"
                  @click="toggleEditItemInfoMenu"
                >
                  <v-icon
                    v-if="isItemInfoEdited"
                    class="unsaved-changes-icon"
                    color="#6495ed"
                    size="8"
                  >$vuetify.icons.circle</v-icon>
                </category-item-btn>
              </v-list-tile>
              <v-list-tile>
                <category-item-btn
                  showTitle
                  iconSize="18"
                  title="Delete item"
                  icon="trash-alt"
                  @click="deleteItem"
                />
              </v-list-tile>
            </v-list>
          </v-menu>
        </v-toolbar-items>
      </v-toolbar>

      <v-layout column class="pa-3">
        <v-flex>
          <v-form @keydown.native.enter.ctrl="updateItemInfo">
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
          </v-form>
        </v-flex>
        <v-flex align-self-end>
          <v-btn
            title="Cancel"
            @click="resetAndToggleEditItemInfoMenu"
          >
            Cancel
          </v-btn>
          <v-btn
            color="info"
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

  resetAndToggleEditItemInfoMenu () {
    this.itemNameEdit = this.itemName
    this.itemLinkEdit = this.itemLink
    this.itemHackageEdit = this.itemHackage
    this.toggleEditItemInfoMenu()
  }

  async updateItemInfo (): Promise<void> {
    if (!this.isInfoSaveEnabled) {
      return
    }
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

<style lang="postcss" scoped>
.category-item-toolbar {
  display: flex;
  margin: 0;
  box-shadow: none;

  >>> {
    .v-toolbar__content {
      justify-content: space-between;
      height: auto !important;
      min-height: 56px;

      .v-toolbar__title {
        flex-wrap: wrap;
        flex: 1;
        overflow: visible;
        padding: 12px 0;
      }

      .spacer {
        display: none;
      }

      .v-toolbar__items {
        height: 100%;
        margin-top: 10px;
        align-self: baseline;
        margin-left: 5px;
      }

      @media only screen and (max-width: 959px) {
        padding: 0 8px;
      }
    }

    .v-expansion-panel__header {
      padding: 0;
      align-items: center;
      cursor: unset;
    }

    .v-expansion-panel__body {
      background: rgb(222, 222, 222);
    }
  }
}
.category-item-toolbar-title {
  display: inline-flex;
}
.category-item-anchor {
  color: rgb(151, 151, 151);
}
.category-item-name-and-badges {
  margin-left: 0.4rem;
}
.category-item-name {
  white-space: pre-line;
  word-break: break-word;
}
.category-item-badges {
  display: inline-block;
  margin-left: 8px;

  > * {
    vertical-align: middle;
    display: inline-flex;
    align-items: center;
    padding: 4px;
    border-radius: 5px;
  }

  .hackage-link {
    background: #5e5184;
    color: #fff;
  }
}
.unsaved-changes-icon {
  position: absolute;
  bottom: 0;
  right: 5px;
}

.category-item-toolbar-btns {
  display: flex;
  align-items: center;
  flex: 1;
}
.category-toolbar-mobile-menu-btn {
  display: none;
  margin: 0;
}

.category-item-toolbar-mobile-menu-list {
  >>> .v-list__tile {
    height: 36px;
    padding: 0 6px;
  }

  >>> button {
    width: 100%;
    padding: 5px;

    .v-btn__content {
      justify-content: flex-start;
    }
  }
}

@media (max-width: 768px) {
  .category-toolbar-mobile-menu-btn {
    display: block;
  }
  .category-item-toolbar-btns {
    display: none;
  }
  .category-item-badges {
    display: block;
    margin: 5px 0 0 0;
  }
  .unsaved-changes-icon {
    right: unset;
    left: 13px;
  }
}
</style>
