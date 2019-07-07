<template>
  <div class="category-top">
    <div class="category-top-titles">
      <v-tooltip bottom>
        <template v-slot:activator="{ on }">
          <a-link
            openInNewTab
            aria-label="RSS feed for all new items in this category"
            :url="`https://guide.aelve.com/haskell/feed/category/${categoryId}`"
            class="category-rss-button"
            v-on="on"
          >
            <v-icon
              left
              size="20"
              class="mr-3 rss-link-icon"
            >$vuetify.icons.rss</v-icon>
          </a-link>
        </template>
        <span>RSS feed for all new items in this category</span>
      </v-tooltip>

      <h2 class="category-name-title mr-3">
        {{categoryTitle}}
      </h2>
      <h2 class="category-group-title"> {{categoryGroup}} </h2>
    </div>

    <v-menu
      bottom
      left
    >
      <template v-slot:activator="{ on }">
        <v-btn
          flat
          icon
          class="category-actions-menu-btn"
          v-on="on"
        >
          <v-icon
            color="grey"
            size="16"
          >$vuetify.icons.bars</v-icon>
        </v-btn>
      </template>

      <v-list>
        <v-list-tile class="category-actions-menu-item">
          <CategoryHeaderBtn
            text="New item"
            icon="plus"
            class="mr-1"
            @click="openAddItemDialog"
          />
        </v-list-tile>
        <v-list-tile class="category-actions-menu-item">
          <CategoryHeaderBtn
            text="Category settings"
            icon="cog"
            class="mr-1"
            @click="openCategorySettingsEditDialog"
          />
        </v-list-tile>
        <v-list-tile class="category-actions-menu-item">
          <CategoryHeaderBtn
            text="Delete category"
            icon="trash-alt"
            @click="deleteCategory"
          />
        </v-list-tile>
      </v-list>
    </v-menu>

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

    <category-info-edit
      v-model="isCategoryInfoEdit"
      :categoryId="categoryId"
    />
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import CategoryInfoEdit from 'client/components/CategoryInfoEdit.vue'
import ALink from 'client/components/ALink.vue'
import Confirm from 'client/helpers/ConfirmDecorator'
import CategoryHeaderBtn from 'client/components/CategoryHeaderBtn.vue'

@Component({
  components: {
    CategoryInfoEdit,
    ALink,
    CategoryHeaderBtn
  }
})
export default class CategoryHeader extends Vue {
  @Prop(Object) category!: object
  @Prop(String) categoryId!: string
  @Prop(String) categoryTitle!: string
  @Prop(String) categoryGroup!: string
  @Prop(String) categoryUrl!: string

  isCategoryInfoEdit: boolean = false
  isAddItemDialogOpen: boolean = false

  openCategorySettingsEditDialog () {
    this.isCategoryInfoEdit = true
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
.category-top {
  display: flex;
  align-items: baseline;
  justify-content: space-between;
  margin: 0 0 10px;
}

.rss-link-icon:hover {
  color: #000;
}

.category-top-titles {
  display: flex;
  flex: 1;
  align-items: baseline;
}

.category-rss-button {
  /* For vertical aligning on one line with category title */
  font-size: 1px;
}

.category-name-title {
  font-size: 22px;
  font-weight: 800;
  text-decoration: none;
  color: #2b2a2a;
  letter-spacing: -1px;
}

.category-group-title {
  font-size: 16px;
  font-weight: normal;
}

.category-actions-menu-btn {
  display: none;
}

.category-actions {
  display: flex;
  align-items: baseline;
  justify-content: center;
  margin-top: 5px;
}

.category-actions-menu-item {
  height: 36px;

  >>> .v-list__tile {
    height: 36px;
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
  .category-actions-menu-btn {
    display: block;
  }
  .category-actions {
    display: none;
  }
}

@media (max-width: 425px) {
  .category-name-title {
    font-size: 18px;
  }
  .category-group-title {
    font-size: 14px;
  }
}
</style>

