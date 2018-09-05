<template>
  <v-container
    grid-list-md 
    text-xs-center
  >
    <v-layout row wrap>
      <v-flex
        class="flex-1"
        v-for="(categories, index) in chunkedCategories" 
        :key="index"
      >
        <div
          class="categoryGroup"
          v-for="(groupCategories, index) in categories" 
          :key="index"
        >
          <h1> {{ groupCategories[0].group }} </h1>
          <a href="#"> 
            <h4 
              class="ml-2" 
              v-for="(category, index) in groupCategories" 
              :key="index"
            > 
              {{ category.title }} 
            </h4>
          </a>
        </div>
      </v-flex>
    </v-layout>
  </v-container>
</template>

<script lang="ts">
import axios from "axios";
import _groupBy from 'lodash/groupBy'
import _chunk from 'lodash/chunk'
import Vue from 'vue'
import Component from 'vue-class-component'

@Component
export default class Index extends Vue {
  categories = []

  async mounted() {
    const { data: categories } = await axios.get("api/categories")
    this.categories = Object.values(_groupBy(categories, 'group'))
  }
  // Returns any number of categories devided to setted number of arrays
  // Its usefull for rendering predifined number of columns 
  get chunkedCategories() {
    const numberOfArrayToDevideTo = 3
    return _chunk(this.categories, Math.ceil(this.categories.length/numberOfArrayToDevideTo))
  }
}
</script>

<style scoped> 
.categoryGroup {
  text-align: left;
}
</style>