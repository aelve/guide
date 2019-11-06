<template>
  <div
    class="category-item-toolbar"
    data-testid="CategoryItemToolbar"
  >
    <div class="category-item-toolbar__header">
      <v-toolbar
        color="#dedede"
        class="elevation-2"
      >
        <v-toolbar-title class="text-h2">
          <span class="category-item-toolbar-title">
            <a
              :href="`#item-${itemUid}`"
              class="category-item-anchor"
            >#</a>

            <div class="category-item-name-and-badges">
              <a
                v-if="itemLink"
                class="category-item-name"
                target="_blank"
                data-testid="CategoryItemToolbar-ItemName"
                :href="itemLink"
              >{{ itemName }}</a>
              <span
                v-else
                data-testid="CategoryItemToolbar-ItemName"
                class="category-item-name"
              >{{ itemName }}</span>
              <div class="category-item-badges">
                <a
                  v-if="this.itemHackage"
                  class="hackage-link"
                  target="_blank"
                  data-testid="CategoryItemToolbar-HackageLink"
                  :href="`https://hackage.haskell.org/package/${this.itemHackage}`"
                >
                  <v-icon
                    color="#fff"
                    size="0.6rem"
                  >$vuetify.icons.link</v-icon>hackage</a>
              </div>
            </div>
          </span>
        </v-toolbar-title>

        <v-spacer></v-spacer>

        <v-toolbar-items>
          <ResponsiveBtnsContainer
            :menuAttach="el"
            class="category-item-toolbar-btns"
          >
            <template #menuBtn="{ on }">
              <v-btn
                icon
                small
                :ripple="false"
                aria-label="Actions"
                v-tooltip="'Actions'"
                class="category-toolbar-mobile-menu-btn"
                data-testid="CategoryItemToolbar-MobileMenuBtn"
                v-on="on"
              >
                <v-icon
                  size="18"
                  color="grey darken-2"
                >$vuetify.icons.bars</v-icon>
              </v-btn>
            </template>

            <CategoryItemBtn
              v-for="btn in actionBtns"
              :key="btn.dataTestId"
              titleTooltip
              size="40px"
              iconSize="18"
              :title="btn.title"
              :data-testid="btn.dataTestId"
              :icon="btn.icon"
              :showUnsavedIcon="btn.showUnsavedIcon"
              @click="btn.clickFunc"
            />

            <template slot="menuItems">
              <CategoryItemBtn
                v-for="btn in actionBtns"
                :key="btn.dataTestId"
                block
                text
                showTitle
                iconSize="18"
                :title="btn.title"
                :data-testid="btn.dataTestId"
                :icon="btn.icon"
                :showUnsavedIcon="btn.showUnsavedIcon"
                @click="btn.clickFunc"
              />
            </template>
          </ResponsiveBtnsContainer>
        </v-toolbar-items>
      </v-toolbar>
    </div>

    <ExpandingPanel
      class="category-item-toolbar__edit-item-info-panel"
      :value="isEditItemInfoMenuOpen"
    >
      <v-layout column class="pa-3">
        <v-flex>
          <v-form
            @keydown.native.enter.ctrl="updateItemInfo"
            @keydown.native.enter.meta="updateItemInfo"
          >
            <v-text-field
              v-model="itemNameEdit"
              data-testid="CategoryItemToolbar-InfoEdit-NameInput"
              label="Name"
            />
            <v-text-field
              v-model="itemHackageEdit"
              data-testid="CategoryItemToolbar-InfoEdit-HackageInput"
              label="Name on Hackage (optional)"
            />
            <v-text-field
              v-model="itemLinkEdit"
              data-testid="CategoryItemToolbar-InfoEdit-LinkInput"
              label="Site (optional)"
            />
          </v-form>
        </v-flex>
        <v-flex align-self-end>
          <v-btn
            class="mr-1"
            aria-label="Cancel"
            @click="resetAndToggleEditItemInfoMenu"
          >
            Cancel
          </v-btn>
          <v-btn
            color="info"
            aria-label="Save"
            :disabled="!isInfoSaveEnabled"
            data-testid="CategoryItemToolbar-InfoEdit-SaveBtn"
            @click="updateItemInfo"
          >
            Save
          </v-btn>
        </v-flex>
      </v-layout>
    </ExpandingPanel>
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import normalizeUrl from 'normalize-url'
import Confirm from 'client/helpers/ConfirmDecorator'
import CategoryItemBtn from 'client/components/CategoryItemBtn.vue'
import ResponsiveBtnsContainer from 'client/components/ResponsiveBtnsContainer.vue'
import ExpandingPanel from 'client/components/ExpandingPanel.vue'

@Component({
  components: {
    CategoryItemBtn,
    ResponsiveBtnsContainer,
    ExpandingPanel
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
  el = null

  get actionBtns () {
    return [
      {
        title: 'Move item up',
        dataTestId: 'CategoryItemToolbar-MoveUpBtn',
        icon: 'arrow-up',
        clickFunc: () => this.moveItem('up')
      },
      {
        title: 'Move item down',
        dataTestId: 'CategoryItemToolbar-MoveDownBtn',
        icon: 'arrow-down',
        clickFunc: () => this.moveItem('down')
      },
      {
        title: 'Edit item info',
        dataTestId: 'CategoryItemToolbar-EditInfoBtn',
        icon: 'cog',
        showUnsavedIcon: this.isItemInfoEdited,
        clickFunc: () => this.toggleEditItemInfoMenu()
      },
      {
        title: 'Delete item',
        icon: 'trash-alt',
        dataTestId: 'CategoryItemToolbar-DeleteBtn',
        clickFunc: () => this.deleteItem()
      }
    ]
  }

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

  mounted () {
    // Cause $el is not reactive we need to set it manually after its available
    this.el = this.$el
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
      'color': 'error',
      'data-testid': 'ItemDeleteDialog-ConfirmBtn'
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
.category-item-toolbar__edit-item-info-panel {
  background: #d6d6d6;
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
  vertical-align: middle;

  > * {
    display: inline-flex;
    align-items: center;
    font-size: 0.7rem;
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

.category-item-toolbar-btns {
  >>> .responsive-bar__desktop-wrap {
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
}
.category-toolbar-mobile-menu-btn {
  /* For some reason vuetify sets important "height: 100%"" for direct child buttons of toolbar */
  height: 1.6rem !important;
  width: 1.6rem !important;
  border-radius: 0;
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
  .category-item-badges {
    display: flex;
    flex-wrap: wrap;
    margin: 6px 0 0 0;
  }
  >>> .v-toolbar__items {
    margin-left: 5px;
    align-self: baseline;
  }
}
</style>
