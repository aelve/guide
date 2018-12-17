<template>
  <div class="category-item">

    <category-item-toolbar
      :itemUid="itemUid"
      :itemName="name"
      :itemLink="link"
      :itemGroup="group"
      :itemKind="kind"
    />

    <div class="category-item-body">
      <p class="category-item-section-title">Summary</p>
      <div
        class="category-item-description"
        v-html="itemDescription"
      />
      <div class="flex-wrapper category-item-section pros-cons-box">
        <div class="width-50">
          <p class="category-item-section-title">Pros</p>
          <ul
            v-if="pros"
            v-for="(value, index) in pros"
            :key="index"
          >
            <li v-html="value.content.html"/>
          </ul>
        </div>
        <div class="width-50">
          <p class="category-item-section-title">Cons</p>
          <ul v-if="cons" v-for="(value, index) in cons" :key="index">
            <li v-html="value.content.html"></li>
          </ul>
        </div>
      </div>
      <div class="category-item-section">
        <p class="category-item-section-title">Ecosystem</p>
        <div v-html="ecosystem" />
      </div>
      <div class="category-item-section notes-box">
        <p class="category-item-section-title">Notes</p>
        <div class="notes-settings">
          <!-- TODO change a to vue markup element -->
          <button class="notes-settings-btn" @click="expandNotes">expand notes</button>
          <button class="notes-settings-btn" @click="collapseNotes">collapse notes</button>
          <button class="notes-settings-btn" style="display: none;">edit notes</button>
        </div>
        <div
          v-for="(value, index) in tocArray"
          :key="index"
        >
          <ul>
            <li
              class="notes-toc-item"
              :key="index"
              v-for="(value, index) in value"
              v-if="value.content"
            >
              <a
                :href="`#${value.slug}`"
                @click="expandNotes"
              >
                <p>{{value.constent.html}}</p>
              </a>
            </li>
          </ul>
        </div>
        <!-- TODO lookslike transition not working -->
        <transition name="slidedown">
          <div
            class="notes-content"
            v-show="isNoteExpanded"
            v-html="notes"
          />
        </transition>
        <!-- <markdown-editor /> -->
      </div>
    </div>
  </div>
</template>

<script lang="ts">
import { Vue, Component, Prop } from 'vue-property-decorator'
import { ICategoryItem } from 'client/service/CategoryItem.ts'
// import MarkdownEditor from 'vue-simplemde/src/markdown-editor.vue'
import CategoryItemToolbar from 'client/components/CategoryItemToolbar.vue'

@Component({
  components: {
    CategoryItemToolbar/* ,
    MarkdownEditor */
  }
})
export default class CategoryItem extends Vue {
  @Prop(String) name!: string
  @Prop(String) group!: string
  @Prop(String) itemDescription!: string
  @Prop(Array) pros!: [any]
  @Prop(Array) cons!: [any]
  @Prop(String) ecosystem!: string
  @Prop(Array) tocArray!: [any]
  @Prop(Object) tocItemContent!: object
  @Prop(String) notes!: string
  @Prop(String) itemUid!: string
  @Prop(String) link!: string
  @Prop(Object) kind!: object

  isNoteExpanded: boolean = false

  expandNotes () {
    this.isNoteExpanded = true
  }

  collapseNotes () {
    this.isNoteExpanded = false
  }
}
</script>

<style scoped>
.category-item-body {
  padding: 15px 20px 25px;
}

.category-item-body >>> p {
  font-size: 16px;
  margin: 0 0 10px;
}

.category-item-body >>> li {
  font-size: 16px;
}

.category-item-description {
  margin: 10px 0 60px;
}

.category-item-description p {
  margin: 0 0 15px;
  font-size: 16px;
}

.category-item-description >>> h1 {
  margin: 25px 0 5px;
}

.category-item-section {
  margin: 30px 0;
}

.notes-box {
  position: relative;
}

.category-item-section.pros-cons-box,
.category-item-section.notes-box >>> li {
  margin: 0 0 5px;
}

.category-item-section.notes-box >>> h1 {
  margin: 20px 0;
}

.category-item {
  background: #e5e5e5;
  margin: 0 0 80px;
}

.flex-wrapper {
  display: flex;
}

.width-50 {
  width: 50%;
  padding-right: 20px;
}

.width-50:nth-last-child(1) {
  padding-right: 0;
  padding-left: 20px;
}

.category-item-section-title {
  display: block;
  margin: 0 0 8px;
  font-size: 22px !important;
  font-weight: 600;
}

.notes-toc-item >>> p {
  margin: 0;
}

.notes-toc-item >>> a {
  text-decoration: none;
  transition: all ease-in-out 0.25s;
}

.notes-toc-item >>> a:hover {
  color: #7eb2e5;
}

.notes-settings {
  display: flex;
  width: 100%;
  padding: 0 0 12px;
}

.notes-settings-btn {
  margin-left: 20px;
  padding: 3px 8px 2px;
  background: #212121;
  border-radius: 4px;
  color: #fff;
  transition: all ease-in-out 0.25s;
}

.notes-settings-btn:hover {
  background: #424242;
}

.notes-settings-btn:focus,
.notes-settings-btn:active {
  outline: none;
}

.notes-settings-btn:nth-child(1) {
  margin-left: 0;
}

.notes-content {
  /* position: absolute; */
  transform-origin: top;
  /* bottom: 0; */
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