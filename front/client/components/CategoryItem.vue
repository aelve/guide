<template>
  <div class="category-item">
    <!-- This "hacky" element used as anchor and located with top offset because of page toolbar which can overlay category item header -->
    <span
      style="position: absolute; top: -96px;"
      :id="`item-${item.id}`"
    />

    <category-item-toolbar
      :itemId="item.id"
      :itemName="item.name"
      :itemLink="item.link"
      :itemHackage="item.hackage"
    />

    <div class="category-item-body">

      <category-item-section
        title="Summary"
        class="mb-3"
        data-testid="CategoryItem-SummarySection"
        :editText="item.summary.text"
        @save="updateSummary({original: item.summary.text, modified: $event})"
      >
        <div
          class="mb-2"
          data-testid="CategoryItem-SummarySection-Content"
          v-html="item.summary.html"
        />
      </category-item-section>

      <div
        v-if="isSectionEnabled('ItemProsConsSection')"
        data-testid="CategoryItem-TraitsSection"
        class="category-item-traits mb-3"
      >
        <category-item-traits
          type="Pro"
          :itemId="item.id"
          :traits="item.pros"
          :isAnyTraitEditing.sync="isPropsEditing"
        />
        <category-item-traits
          type="Con"
          :itemId="item.id"
          :traits="item.cons"
          :isAnyTraitEditing.sync="isConsEditing"
        />
      </div>

      <category-item-section
        v-if="isSectionEnabled('ItemEcosystemSection')"
        title="Ecosystem"
        class="mb-3"
        data-testid="CategoryItem-EcosystemSection"
        :editText="item.ecosystem.text"
        @save="updateEcosystem({original: item.ecosystem.text, modified: $event})"
        @toggleEdit="toggleItemEcosystemEditState"
      >
        <div
          v-html="item.ecosystem.html" 
          data-testid="CategoryItem-EcosystemSection-Content"
        />
      </category-item-section>

      <category-item-section
        v-if="isSectionEnabled('ItemNotesSection')"
        title="Notes"
        class="mb-3"
        data-testid="CategoryItem-NotesSection"
        :editText="item.notes.text"
        @save="updateNotes({original: item.notes.text, modified: $event})"
        @toggleEdit="toggleItemNotesEditState"
      >
        <v-btn
          small
          dark
          rounded
          :aria-label="areNotesExpanded ? 'Collapse' : 'Expand'"
          class="mx-0"
          @click="toggleNotes"
        >
          {{ areNotesExpanded ? 'Collapse' : 'Expand' }}
        </v-btn>

        <ul>
          <li
            v-for="(tocItem, index) in item.toc"
            :key="index"
            style="position: relative;"
          >
            <span
              style="position: absolute; top: -96px;"
              :id="`item-${item.id}`"
            />
            <a
              :href="`#${tocItem.slug}`"
              v-html="tocItem.content.html"
              @click="expandNotes"
            />
          </li>
        </ul>

        <v-slide-y-transition hide-on-leave>
          <div
            v-show="areNotesExpanded"
            class="category-item__notes"
          >
            <div
              v-if="item.notes.html"
              v-html="item.notes.html"
              data-testid="CategoryItem-NotesSection-Content"
            />
            <span v-else> &lt;notes are empty&gt; </span>
          </div>
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
import { ICategoryItem } from 'client/service/CategoryItem'

@Component({
  components: {
    CategoryItemToolbar,
    CategoryItemSection,
    CategoryItemTraits
  },
  mixins: [conflictDialogMixin]
})
export default class CategoryItem extends Vue {
  @Prop(Object) item: ICategoryItem
  @Prop(Array) sections!: string[]

  areNotesExpanded: boolean = false
  isPropsEditing: boolean = false
  isConsEditing: boolean = false

  get isAnyTraitEditing () {
    return this.isPropsEditing || this.isConsEditing
  }

  @Watch('isAnyTraitEditing', { immediate: true })
  updateItemTraitEditingState (newVal, prevVal) {
    if (!!newVal !== !!prevVal) {
      this.$store.dispatch('category/toggleItemProsConsSectionEdit', this.item.id)
    }
  }

  isSectionEnabled (section) {
    return this.sections.includes(section)
  }

  toggleNotes (): void {
    this.areNotesExpanded = !this.areNotesExpanded
  }

  expandNotes () {
    this.areNotesExpanded = true
  }

  toggleItemEcosystemEditState () {
    this.$store.dispatch('category/toggleItemEcosystemSectionEdit', this.item.id)
  }

  toggleItemNotesEditState () {
    this.$store.dispatch('category/toggleItemNotesSectionEdit', this.item.id)
  }

  @CatchConflictDecorator
  async updateSummary ({ original, modified }): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemSummary', {
      id: this.item.id,
      original,
      modified
    })
    await this.$store.dispatch('category/reloadItem', { id: this.item.id })
  }

  @CatchConflictDecorator
  async updateEcosystem ({ original, modified }): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemEcosystem', {
      id: this.item.id,
      original,
      modified
    })
    await this.$store.dispatch('category/reloadItem', { id: this.item.id })
  }

  @CatchConflictDecorator
  async updateNotes ({ original, modified }): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemNotes', {
      id: this.item.id,
      original,
      modified
    })
    await this.$store.dispatch('category/reloadItem', { id: this.item.id })
  }
}
</script>

<style lang="postcss" scoped>
.category-item {
  position: relative;
  background: #eeeeee;
  margin: 0 0 40px;
  border-radius: 4px;
}

.category-item-body {
  padding: 15px 20px;
}

.category-item-traits {
  display: flex;
  flex-wrap: wrap;
}

.category-item-traits > * {
  flex: 1;
  min-width: calc(50% - 20px);
}

.category-item-traits > *:not(:last-child) {
  margin-right: 20px;

  @media screen and (max-width: 768px) {
    margin-right: 0;
  }
}

.category-item__notes >>> {
  span[id^="item-notes-"] {
    /* Top offstet is required because of app toolbar */
    position: relative;
    top: -74px;
  }
}

@media screen and (max-width: 768px) {
  .category-item-body {
    width: 100%;
  }
  .category-item {
    margin: 0 0 30px;
  }
  .category-item-traits {
    flex-direction: column;
  }
}
</style>
