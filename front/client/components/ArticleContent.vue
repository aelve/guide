<template>
  <div class="article-item">
    <div class="article-header">
      <p class="article-hd-textlg">{{ kind }}</p>
      <a-link 
        openInNewTab :url="`http://hackage.haskell.org/package/${kind}`" 
        class="article-header-link">
        (Hackage)
      </a-link>
      <p class="article-hd-textsm">{{ group }}</p>
      <div class="article-header-icons">
        <i class="fas fa-arrow-up"></i>
        <i class="fas fa-arrow-down"></i>
        <div class="header-func-icons">
          <i class="fas fa-cogs"></i>
<<<<<<< HEAD
          <button
            class="item-del-btn" 
            @click="openConfirmDialog"
          >
=======
          <button @click="openConfirmDialog">
>>>>>>> 2b47740b86f9098a71f633c54b844288c98479fb
            <i class="fas fa-times"></i>
          </button>
        </div>
      </div>
    </div>
    <div class="article-content">
      <p class="article-section-title">Summary</p>
      <div 
        class="article-description" 
        v-html="itemDescription"/>
      <div class="flex-wrapper article-section pros-cons-box">
        <div class="width-50">
          <p class="article-section-title">Pros</p>
          <ul 
            v-if="pros" 
            v-for="(value, index) in pros" 
            :key="index">
            <li v-html="value.content.html"/>
          </ul>
        </div>
        <div class="width-50">
          <p class="article-section-title">Cons</p>
          <ul v-if="cons" v-for="(value, index) in cons" :key="index">
            <li v-html="value.content.html"></li>
          </ul>
        </div>
      </div>
      <div class="article-section">
        <p class="article-section-title">Ecosystem</p>
        <div v-html="ecosystem"></div>
      </div>
      <div class="article-section notes-box">
        <p class="article-section-title">Notes</p>
        <div class="notes-settings">
          <!-- TODO change a to vue markup element -->
          <button class="notes-settings-btn" @click="expandNotes">expand notes</button>
          <button class="notes-settings-btn" @click="collapseNotes">collapse notes</button>
          <button class="notes-settings-btn" style="display: none;">edit notes</button>
        </div>
        <div 
          v-for="(value, index) in tocArray" 
          :key="index">
          <!-- TODO refactor v-for from ul to li -->
          <ul 
            v-for="(value, index) in value" 
            :key="index">
            <li 
              class="notes-toc-item" 
              v-if="value.content">
              <a 
                :href="`#${value.slug}`" 
                @click="expandNotes">
                <p>{{value.content.html}}</p>
              </a>
            </li>
          </ul>
        </div>
        <transition name="slidedown">
          <div
            class="notes-content"
            v-show="isNoteExpanded" 
            v-html="notes">
          </div>
        </transition>
      </div>
    </div>
    <confirm-dialog 
      v-model="isConfirmDialogOpen"
      :confirmationText="'delete this item'"
      :confirmAction="deleteArticleContent"
      :itemId="itemUid"
    />
    <!-- Shit happens because passing function as a prop somehow calls this function -->
  </div>
</template>

<script lang="ts">
// import Vue from 'vue'
// import Component from 'vue-class-component'
import { Vue, Component, Prop, Watch } from 'vue-property-decorator'
import ConfirmDialog from 'client/components/ConfirmDialog.vue'

@Component({
  components: {
    ConfirmDialog
  }
})

export default class ArticleContent extends Vue {
  @Prop(String) kind!: string
  @Prop(String) group!: string
  @Prop(String) itemDescription!: string
  @Prop(Array)  pros!: [any]
  @Prop(Array)  cons!: [any]
  @Prop(String) ecosystem!: string
  @Prop(Array) tocArray!: [any]
  @Prop(Object) tocItemContent!: object
  @Prop(String) notes!: string
  @Prop(String) itemUid!: string

  isNoteExpanded: boolean = false
  isConfirmDialogOpen: boolean = false

  expandNotes() {  
    this.isNoteExpanded = true
  }

  collapseNotes() {
    this.isNoteExpanded = false
  }

  async deleteArticleContent(itemId: any) {
<<<<<<< HEAD
    const deleteContent = await this.$store.dispatch('categoryItem/deleteItem', {
=======
    await this.$store.dispatch('categoryItem/deleteItem', {
>>>>>>> 2b47740b86f9098a71f633c54b844288c98479fb
      id: itemId
    })
  }

  openConfirmDialog() {
    this.isConfirmDialogOpen = true
  }
} 
</script>

<style scoped>
  .article-content >>> p {
    font-size: 16px;
    margin: 0 0 10px;
  }

  .article-content >>> li {
    font-size: 16px;
  }

  .article-description {
    margin: 10px 0 60px;
  }

  .article-description >>> p {
    margin: 0;
  }

  .article-description >>> p {
    margin: 0 0 15px;
    font-size: 16px;
  }

  .article-description >>> h1 {
    margin: 25px 0 5px;
  }

  .article-section {
    margin: 30px 0;
  }

  .notes-box {
    position: relative;
  }

  .article-section.pros-cons-box, .article-section.notes-box >>> li {
    margin: 0 0 5px;
  }

  .article-section.notes-box >>> h1 {
    margin: 20px 0;
  }

  .article-item {
    background: #E5E5E5;
    padding: 15px 20px 25px;
    margin: 0 0 80px;
  }

  .article-header {
    display: flex;
    align-items: center;
    padding: 10px 15px;
    margin: -15px -20px 15px;
    background: #C8C8C8;
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

  .article-section-title {
    display: block;
    margin: 0 0 8px;
    font-size: 22px!important;
    font-weight: 600;
  }

  .article-hd-textlg {
    font-size: 22px;
  }

  .article-hd-textsm {
    font-size: 18px;
  }

  .article-header-link {
    font-size: 22px;
    padding: 0 32px 0 8px;
  }

  .article-header-icons {
    display: flex;
    align-items: center;
    justify-content: flex-end;
    flex: 1;
  }

  .article-header-icons >>> i {
    margin-right: 5px;
    font-size: 18px;
    color: #979797;
    cursor: pointer;
    transition: all ease-in-out .25s;
  }

  .article-header-icons >>> i:nth-last-child(1) {
    margin: 0;
  }

  .article-header-icons >>> i:hover {
    color: #000;
  }

  .header-func-icons {
    padding-left: 20px;
  }

  .notes-toc-item >>> p {
    margin: 0;
  }

  .notes-toc-item >>> a {
    text-decoration: none;
    transition: all ease-in-out .25s;
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
    transition: all ease-in-out .25s;
  }

  .notes-settings-btn:hover {
    background: #424242;
  }

  .notes-settings-btn:focus, .notes-settings-btn:active {
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
    .article-content {
      width: 100%;
    }
    .article-item {
      margin: 0 0 30px;
    }
    .article-hd-textlg {
      font-size: 20px;
    }
    .article-hd-textsm {
      font-size: 16px;
    }
    .article-header-link {
      font-size: 20px;
      padding: 0 32px 0 8px;
    }
  }
</style>
