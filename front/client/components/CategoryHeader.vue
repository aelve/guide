<template>
  <div class="category-header">
    <div class="category-header__first-row">
      <a
        class="rss-link"
        :href="`https://guide.aelve.com/haskell/feed/category/${categoryId}`"
        target="_blank"
        aria-label="RSS feed for all new items in this category"
        v-tooltip.bottom-start="{ content: 'RSS feed for all new items in this category'}"
      >
        <v-icon class="rss-link-icon">$vuetify.icons.rss</v-icon>
      </a>
      <h1 class="category-name-title" :title="categoryTitle">{{categoryTitle}}</h1>
    </div>

    <div class="category-header__second-row">
      <div class="category-group-title-wrap">
        in <span :title="categoryGroup" class="category-group-title"> {{ categoryGroup }} </span>
      </div>

      <div class="category-actions">
        <CategoryHeaderBtn
          text="New item"
          icon="plus"
          class="mr-1"
          @click="openAddItemDialog"
        />
        <CategoryHeaderBtn
          text="Category settings"
          icon="cog "
          class="mr-1"
          @click="openCategorySettingsEditDialog"
        />
        <CategoryHeaderBtn
          text="Delete category"
          icon="trash-alt"
          @click="deleteCategory"
        />
      </div>

      <v-menu bottom left offset-y>
        <template v-slot:activator="{ on }">
          <v-btn
            text
            icon
            aria-label="Actions"
            v-tooltip="'Actions'"
            class="category-actions-menu-btn"
            v-on="on"
          >
            <v-icon
              color="grey darken-2"
              size="18"
            >$vuetify.icons.bars</v-icon>
          </v-btn>
        </template>

        <v-list class="category-actions-menu-list">
          <v-list-item>
            <CategoryHeaderBtn
              block 
              text="New item"
              icon="plus"
              @click="openAddItemDialog"
            />
          </v-list-item>
          <v-list-item>
            <CategoryHeaderBtn
              block 
              text="Category settings"
              icon="cog"
              @click="openCategorySettingsEditDialog"
            />
          </v-list-item>
          <v-list-item>
            <CategoryHeaderBtn
              block
              text="Delete category"
              icon="trash-alt"
              @click="deleteCategory"
            />
          </v-list-item>
        </v-list>
      </v-menu>

    </div>
    

    <CategorySettingsDialog
      v-model="isCategorySettingsDialog"
      :categoryId="categoryId"
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

@Component({
  components: {
    CategorySettingsDialog,
    CategoryHeaderBtn
  }
})
export default class CategoryHeader extends Vue {
  @Prop(Object) category!: object
  @Prop(String) categoryId!: string
  @Prop(String) categoryTitle!: string
  @Prop(String) categoryGroup!: string
  @Prop(String) categoryUrl!: string

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
      color: 'error'
    }
  })
  async deleteCategory () {
    if (!this.category) {
      return
    }
    await this.$store.dispatch('category/deleteCategory', this.categoryId)
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
  vertical-align: top;
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

.category-actions-menu-btn {
  display: none;
  margin: 0;
  width: 36px;
  height: 36px;
}

.category-actions {
  white-space: nowrap;
  flex: 1;
}

.category-actions-menu-list {
  >>> .v-list-item {
    height: 36px;
    padding: 0;
  }

  >>> button {
    height: 100% !important;
    padding: 0 6px;

    .v-btn__content {
      justify-content: flex-start;
    }
  }
}

@media (max-width: 768px) {
  .category-actions-menu-btn {
    display: block;
  }
  .category-actions {
    display: none;
  }
}
</style>

