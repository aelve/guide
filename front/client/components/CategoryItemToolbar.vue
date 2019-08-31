<template>
  <div class="category-item-toolbar">
    <div class="category-item-toolbar__header">
      <v-toolbar
        color="#dedede"
        class="elevation-2"
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
                    size="10"
                  >$vuetify.icons.link</v-icon>hackage</a>
              </div>
            </div>
          </span>
        </v-toolbar-title>

        <v-spacer></v-spacer>

        <v-toolbar-items>
          <div class="category-item-toolbar-btns">
            <category-item-btn
              size="40px"
              iconSize="18"
              title="Move item up"
              icon="arrow-up"
              @click="moveItem('up')"
            />
            <category-item-btn
              size="40px"
              iconSize="18"
              title="Move item down"
              icon="arrow-down"
              @click="moveItem('down')"
            />
            <category-item-btn
              size="40px"
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
              size="40px"
              iconSize="18"
              title="Delete item"
              icon="trash-alt"
              @click="deleteItem"
            />
          </div>

          <v-menu bottom left offset-y>
            <template v-slot:activator="{ on }">
              <v-btn
                icon
                small
                :ripple="false"
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

            <v-list class="category-item-toolbar__mobile-menu-list">
              <v-list-item>
                <category-item-btn
                  block
                  text
                  showTitle
                  iconSize="18"
                  title="Move item up"
                  icon="arrow-up"
                  @click="moveItem('up')"
                />
              </v-list-item>
              <v-list-item>
                <category-item-btn
                  block
                  text
                  showTitle
                  iconSize="18"
                  title="Move item down"
                  icon="arrow-down"
                  @click="moveItem('down')"
                />
              </v-list-item>
              <v-list-item>
                <category-item-btn
                  block
                  text
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
              </v-list-item>
              <v-list-item>
                <category-item-btn
                  block
                  text
                  showTitle
                  iconSize="18"
                  title="Delete item"
                  icon="trash-alt"
                  @click="deleteItem"
                />
              </v-list-item>
            </v-list>
          </v-menu>
        </v-toolbar-items>
      </v-toolbar>
    </div>

    <div
      class="category-item-toolbar__expanding-content"
      :class="{ 'category-item-toolbar__expanding-content_expanded': isEditItemInfoMenuOpen }"
      :aria-hidden="!isEditItemInfoMenuOpen"
    >
      <v-layout column class="pa-3">
        <v-flex>
          <v-form
            @keydown.native.enter.ctrl="updateItemInfo"
            @keydown.native.enter.meta="updateItemInfo"
          >
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
            class="mr-1"
            title="Cancel"
            @click="resetAndToggleEditItemInfoMenu"
          >
            Cancel
          </v-btn>
          <v-btn
            color="info"
            title="Save"
            :disabled="!isInfoSaveEnabled"
            @click="updateItemInfo"
          >
            Save
          </v-btn>
        </v-flex>
      </v-layout>
    </div>
  </div>
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
    return this.itemLink === trimmed ? this.itemLink : normalizeUrl(trimmed, { stripWWW: false })
  }

  @Confirm({
    text: 'delete this item',
    confirmBtnText: 'Delete',
    confirmBtnProps: {
      color: 'error'
    }
  })
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
/* TODO move expanding panel to external component */
.category-item-toolbar__expanding-content {
  max-height: 0;
  overflow: hidden;
  background: #d6d6d6;
  transition: max-height 0.2s ease-in-out;
}
.category-item-toolbar__expanding-content_expanded {
  max-height: 300px;
}
.category-item-toolbar__header >>> {
  .v-toolbar__title {
    overflow: visible;
  }

  .v-toolbar__content {
    justify-content: space-between;
    /* Vuetify sets inline height style */
    height: auto !important;
    align-items: flex-start;
    padding: 8px 16px;

    .spacer {
      display: none;
    }

    .v-toolbar__items {
      margin-left: 15px;
    }

    @media screen and (max-width: 959px) {
      padding: 8px;
    }
  }
}
.category-item-toolbar-title {
  display: flex;
  line-height: 1;
}
a.category-item-anchor {
  color: #616161;
}
.category-item-name-and-badges {
  margin-left: 0.4rem;
}
.category-item-name {
  white-space: pre-line;
  word-break: break-word;

  /* A gap beetwen category item name and badges */
  @media (min-width: 768px) {
    &:after {
      content: " ";
      visibility: hidden;
    }
  }
}
.category-item-badges {
  display: inline-flex;
  align-items: center;
  vertical-align: bottom;

  > * {
    display: inline-flex;
    align-items: center;
    line-height: 1;
    border-radius: 5px;
    padding: 4px 6px;
  }

  .hackage-link {
    background: #5e5184;
    color: #fff;

    svg {
      margin-right: 2px;
    }
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

  > * {
    width: 1.6rem !important;
    height: 1.6rem !important;
  }

  > *:not(:last-child) {
    margin-right: 6px;
  }
}
.category-toolbar-mobile-menu-btn {
  display: none;
  /* Somewhy vuetify sets important "height: 100%"" for direct child buttons of toolbar */
  height: 1.6rem !important;
  width: 1.6rem !important;
  margin: 0;
}

.category-item-toolbar__mobile-menu-list {
  >>> .v-list-item {
    height: 36px;
    padding: 0;

    button {
      height: 100% !important;
      padding: 0 6px;

      .v-btn__content {
        justify-content: flex-start;
      }
    }
  }
}

@media (max-width: 768px) {
  .category-toolbar-mobile-menu-btn {
    display: flex;
  }
  .category-item-toolbar-btns {
    display: none;
  }
  .category-item-badges {
    display: flex;
    flex-wrap: wrap;
    margin: 6px 0 0 0;
  }
  .unsaved-changes-icon {
    right: unset;
    left: 13px;
  }
  >>> .v-toolbar__items {
    margin-left: 5px;
    align-self: baseline;
  }
}
</style>
