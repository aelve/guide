<template>
  <v-container>
    <div v-html="getCategoryDescription"></div>
    <br>
    <br>
    <br>
    <div v-for="(ecosystem, group, name, notes, pros, description, cons, index) in getCategory" :key="index"
      class="category-item">
      <div class="category-item">
        <!-- {{ description.html }}
        <br> -->
        {{ ecosystem.html }}
      </div>
    </div>
  </v-container>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'


@Component
export default class ArticleItem extends Vue {

  async asyncData() {
    return this.$store.dispatch('categoryItem/loadCategoryItem')
  }

  get getCategory () {
    return this.$store.state.categoryItem.categoryItemList
  }

  get getCategoryDescription() {
    const description = this.$store.state.categoryItem.categoryItemList.description.html;
    return description
  }

  get getCategoryItems() {
    const items = this.$store.state.categoryItem.categoryItemList.items
    const parsedItems = items.map((item:any) => {return item.ecosystem.html})

    return parsedItems
  }
}
</script>

<style scoped>
  .category-item {
    background: #878787;
    padding: 15px 20px;
    margin: 0 0 30px;
  }
</style>

