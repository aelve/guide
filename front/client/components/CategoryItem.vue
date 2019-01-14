<template>
  <div class="category-item">

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
        @save="updateSummary"
      >
        <div
          class="mb-2 category-item-summary"
          v-html="summary.html"
        />
      </category-item-section>

      <div class="category-item-traits">
        <category-item-traits
          type="pro"
          :itemId="itemUid"
          :traits="pros"
        />
        <category-item-traits
          type="con"
          :itemId="itemUid"
          :traits="cons"
        />
      </div>

      <category-item-section
        title="Ecosystem"
        :editText="ecosystem.text"
        @save="updateEcosystem"
      >
        <div v-html="ecosystem.html" />
      </category-item-section>

      <category-item-section
        title="Notes"
        :editText="notes.text"
        @save="updateNotes"
      >
        <v-btn
          small
          dark
          round
          class="mx-0"
          @click="expandNotes"
        >
          expand
        </v-btn>
        <v-btn
          small
          dark
          round
          class="mx-0"
          @click="collapseNotes"
        >
          collapse
        </v-btn>

        <ul>
          <li
            v-for="(value, index) in toc"
            :key="index"
          >
            <a
              :href="`#${value[0].slug}`"
              v-html="value[0].content.html"
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
import { Vue, Component, Prop } from 'vue-property-decorator'
import { ICategoryItem } from 'client/service/CategoryItem.ts'
import CategoryItemToolbar from 'client/components/CategoryItemToolbar.vue'
import CategoryItemSection from 'client/components/CategoryItemSection.vue'
import CategoryItemTraits from 'client/components/CategoryItemTraits.vue'

@Component({
  components: {
    CategoryItemToolbar,
    CategoryItemSection,
    CategoryItemTraits
  }
})
export default class CategoryItem extends Vue {
  // TODO get rid of so many props and pass the item fully
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

  isNoteExpanded: boolean = false

  expandNotes (): void {
    this.isNoteExpanded = true
  }

  collapseNotes (): void {
    this.isNoteExpanded = false
  }

  async updateSummary (newValue: string): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemSummary', {
      id: this.itemUid,
      original: this.summary.text,
      modified: newValue
    })
    await this.$store.dispatch('category/reloadCategory')
  }

  async updateEcosystem (newValue: string): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemEcosystem', {
      id: this.itemUid,
      original: this.ecosystem.text,
      modified: newValue
    })
    await this.$store.dispatch('category/reloadCategory')
  }

  async updateNotes (newValue: string): Promise<void> {
    await this.$store.dispatch('categoryItem/updateItemNotes', {
      id: this.itemUid,
      original: this.notes.text,
      modified: newValue
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
  margin: 0 0 10px;
}

.category-item-body >>> li {
  font-size: 16px;
}

.category-item-summary >>> h1 {
  margin: 25px 0 5px;
}

.category-item {
  background: #e5e5e5;
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
