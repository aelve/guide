<template>
  <div class="category-item">
    <!-- This "hacky" element used as anchor and located with top offset because of page toolbar which can overlay category item header -->
    <span
      style="position: absolute; top: -96px;"
      :id="`item-${itemUid}`"
    />

    <category-item-toolbar
      :itemUid="itemUid"
      :itemName="name"
      :itemLink="link"
      :itemGroup="group"
      :itemHackage="hackage"
    />

    <div class="category-item-body">

      <category-item-section
        title="Summary"
        :editText="summary.text"
        @save="updateSummary({original: summary.text, modified: $event})"
      >
        <div
          class="mb-2 category-item-summary"
          v-html="summary.html"
        />
      </category-item-section>

      <div
        v-if="isSectionEnabled('ItemProsConsSection')"
        class="category-item-traits"
      >
        <category-item-traits
          type="Pro"
          :itemId="itemUid"
          :traits="pros"
          :isAnyTraitEditing.sync="isPropsEditing"
        />
        <category-item-traits
          type="Con"
          :itemId="itemUid"
          :traits="cons"
          :isAnyTraitEditing.sync="isConsEditing"
        />
      </div>

      <category-item-section
        v-if="isSectionEnabled('ItemEcosystemSection')"
        title="Ecosystem"
        :editText="ecosystem.text"
        @save="updateEcosystem({original: ecosystem.text, modified: $event})"
        @toggleEdit="toggleItemEcosystemEditState"
      >
        <div v-html="ecosystem.html" />
      </category-item-section>

      <category-item-section
        v-if="isSectionEnabled('ItemNotesSection')"
        title="Notes"
        :editText="notes.text"
        @save="updateNotes({original: notes.text, modified: $event})"
        @toggleEdit="toggleItemNotesEditState"
      >
        <v-btn
          small
          dark
          round
          title="Expand"
          class="mx-0"
          @click="expandNotes"
        >
          Expand
        </v-btn>
        <v-btn
          small
          dark
          round
          title="Collapse"
          class="mx-0"
          @click="collapseNotes"
        >
          Collapse
        </v-btn>

        <ul>
          <li
            v-for="(tocItem, index) in toc"
            :key="index"
          >
            <a
              :href="`#${tocItem.slug}`"
              v-html="tocItem.content.html"
              @click="expandNotes"
            />
          </li>
        </ul>

        <v-slide-y-transition hide-on-leave>
          <div
            v-show="isNoteExpanded"
            v-html="notes.html"
          />
        </v-slide-y-transition>
      </category-item-section>

    </div>
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import CategoryItemToolbar from 'client/components/CategoryItemToolbar.vue'
import CategoryItemSection from 'client/components/CategoryItemSection.vue'
import CategoryItemTraits from 'client/components/CategoryItemTraits.vue'
import conflictDialogMixin from 'client/mixins/conflictDialogMixin'
import CatchConflictDecorator from 'client/helpers/CatchConflictDecorator'

@Component({
  components: {
    CategoryItemToolbar,
    CategoryItemSection,
    CategoryItemTraits
  },
  mixins: [conflictDialogMixin]
})
export default class CategoryItem extends Vue {
  // TODO get rid of so many props get data from Vuex
  @Prop(String) name!: string
  @Prop(String) group!: string
  @Prop(Object) summary!: { text: string, html: string }
  @Prop(Array) pros!: any[]
  @Prop(Array) cons!: any[]
  @Prop(Object) ecosystem!: { text: string, html: string }
  @Prop(Array) toc!: any[]
  @Prop(Object) tocItemContent!: object
  @Prop(Object) notes!: { text: string, html: string }
  @Prop(String) itemUid!: string
  @Prop(String) link!: string
  @Prop(String) hackage!: string
  @Prop(Array) sections!: string[]

  isNoteExpanded: boolean = false
  isPropsEditing: boolean = false
  isConsEditing: boolean = false

  get isAnyTraitEditing () {
    return this.isPropsEditing || this.isConsEditing
  }

  @Watch('isAnyTraitEditing', { immediate: true })
  updateItemTraitEditingState (newVal, prevVal) {
    if (newVal !== prevVal) {
      this.$store.dispatch('category/toggleItemProsConsSectionEdit', this.itemUid)
    }
  }

  isSectionEnabled (section) {
    return this.sections.includes(section)
  }

  expandNotes (): void {
    this.isNoteExpanded = true
  }

  collapseNotes (): void {
    this.isNoteExpanded = false
  }

  toggleItemEcosystemEditState () {
    this.$store.dispatch('category/toggleItemEcosystemSectionEdit', this.itemUid)
  }

  toggleItemNotesEditState () {
    this.$store.dispatch('category/toggleItemNotesSectionEdit', this.itemUid)
  }

  @CatchConflictDecorator
  async updateSummary ({ original, modified }): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemSummary', {
      id: this.itemUid,
      original,
      modified
    })
    await this.$store.dispatch('category/reloadCategory')
  }

  @CatchConflictDecorator
  async updateEcosystem ({ original, modified }): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemEcosystem', {
      id: this.itemUid,
      original,
      modified
    })
    await this.$store.dispatch('category/reloadCategory')
  }

  @CatchConflictDecorator
  async updateNotes ({ original, modified }): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemNotes', {
      id: this.itemUid,
      original,
      modified
    })
    await this.$store.dispatch('category/reloadCategory')
  }
}
</script>

<style scoped>
.category-item-body {
  padding: 15px 20px;
}

.category-item-body >>> p {
  font-size: 16px;
}

.category-item-body >>> li {
  font-size: 16px;
}

.category-item-summary >>> h1 {
  margin: 25px 0 5px;
}

.category-item {
  position: relative;
  background: #eeeeee;
  margin: 0 0 40px;
}

.category-item-traits {
  display: flex;
}

.category-item-traits > * {
  flex: 1;
}

.category-item-traits > *:not(:last-child) {
  margin-right: 20px;
}

@media screend and (max-width: 768px) {
  .category-item-body {
    width: 100%;
  }
  .category-item {
    margin: 0 0 30px;
  }
}
</style>
