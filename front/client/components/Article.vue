<template>
  <v-container>
    <div v-html="getCategoryDescription"></div>
    <br>
    <br>
    <br>
    <p> {{ getCategory }} </p>
    <div v-for="(ecosystem, index) in getCategory" :key="index">
      {{ ecosystem.description }}
    </div>
  </v-container>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'


@Component
export default class ArticleItem extends Vue {

  async asyncData({ store }) {
    return store.dispatch('categoryItem/loadCategoryItem')
  }

  get getCategory () {
    return typeof this.$store.state.categoryItem.categoryItemList
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
