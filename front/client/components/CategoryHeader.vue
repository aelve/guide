<template>
  <div class="category-header">
    <div class="category-header__first-row">
      <a
        class="rss-link"
        :href="`https://guide.aelve.com/haskell/feed/category/${category.id}`"
        target="_blank"
        aria-label="RSS feed for all new items in this category"
        v-tooltip.bottom-start="{ content: 'RSS feed for all new items in this category'}"
      >
        <v-icon class="rss-link-icon">$vuetify.icons.rss</v-icon>
      </a>
      <h1
        class="category-name-title" 
        data-testid="CategoryHeader-Title"
        :title="category.title"
      >{{category.title}}</h1>
    </div>

    <div class="category-header__second-row">
      <div class="category-group-title-wrap">
        in
        <span 
          class="category-group-title"
          data-testid="CategoryHeader-Group"
          :title="category.group"
        >{{ category.group }}</span>
      </div>

      <ResponsiveBtnsContainer>
        <template #menuBtn="{ on }">
          <v-btn
            text
            icon
            aria-label="Actions"
            v-tooltip="'Actions'"
            data-testid="CategoryHeader-MobileMenuBtn"
            class="category-actions-mobile-menu-btn"
            v-on="on" 
          >
            <v-icon
              color="grey darken-2"
              size="18"
            >$vuetify.icons.bars</v-icon>
          </v-btn>
        </template>

        <CategoryHeaderBtn
          class="mr-1"
          text="New item"
          icon="plus"
          data-testid="CategoryHeader-NewItemBtn"
          @click="openAddItemDialog"
        />
        <CategoryHeaderBtn
          class="mr-1"
          text="Category settings"
          icon="cog"
          data-testid="CategoryHeader-CategorySettingsBtn"
          @click="openCategorySettingsEditDialog"
        />
        <CategoryHeaderBtn
          text="Delete category"
          icon="trash-alt"
          data-testid="CategoryHeader-CategoryDeleteBtn"
          @click="deleteCategory"
        />
      </ResponsiveBtnsContainer>
    </div>
    

    <CategorySettingsDialog
      v-model="isCategorySettingsDialog"
      :category="category"
    />
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import CategorySettingsDialog from 'client/components/CategorySettingsDialog.vue'
import CategoryHeaderBtn from 'client/components/CategoryHeaderBtn.vue'
import Confirm from 'client/helpers/ConfirmDecorator'
import { ICategoryFull } from 'client/service/Category'
import ResponsiveBtnsContainer from 'client/components/ResponsiveBtnsContainer.vue'

@Component({
  components: {
    CategorySettingsDialog,
    CategoryHeaderBtn,
    ResponsiveBtnsContainer
  }
})
export default class CategoryHeader extends Vue {
  @Prop(Object) category!: ICategoryFull

  isCategorySettingsDialog: boolean = false
  isAddItemDialogOpen: boolean = false

  openCategorySettingsEditDialog () {
    this.isCategorySettingsDialog = true
  }

  openAddItemDialog () {
    this.$emit('openAddItemDialog')
  }

  @Confirm({
    text: 'delete this category',
    confirmBtnText: 'Delete',
    confirmBtnProps: {
      'color': 'error',
      'data-testid': 'DeleteCategoryDialog-ConfirmBtn'
    }
  })
  async deleteCategory () {
    if (!this.category) {
      return
    }
    await this.$store.dispatch('category/deleteCategory', this.category.id)
    this.$router.back()
  }
}
</script>

<style lang="postcss" scoped>
.category-header {
  border-bottom: 1px solid rgb(221, 221, 221);
  margin-bottom: 20px;
  padding-bottom: 5px;

  /* Vuetify's v-menu component for some reason adds excess div next to v-menu activator button and this div is messing flex layout */
  >>> .v-menu {
    display: none;
  }
}
.category-name-title {
  font-size: 1.9rem;
  font-weight: 700;
  margin: 0 0 5px;
  letter-spacing: -1px;
  display: inline;
}

.rss-link {
  margin-right: 2px;
  height: calc(1.8rem - 5px);
  display: inline-block;
  vertical-align: super;
}

.rss-link-icon {
  height: calc(1.8rem - 5px) !important;

  &:hover {
    color: #000;
  }
}

.category-header__second-row {
  display: flex;
  align-items: center;
  justify-content: space-between;
}

.category-group-title-wrap {
  padding-left: 4px;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  flex: 1;
}

.category-group-title {
  font-weight: 600;
}

.category-actions-mobile-menu-btn {
  margin: 0;
  width: 36px;
  height: 36px;
}

@media (max-width: 768px) {
  .category-header__second-row {
    /* Cause menu btn appears after page loading and causes second row to jump */
    min-height: 36px;
  }
}
</style>

