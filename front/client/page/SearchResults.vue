<template>
  <v-container class="search-result-container">
    <template v-if="results && results.length">

      <template v-if="resultsCategories && resultsCategories.length">
        <h1 class="mt-0 mb-6"> Categories </h1>

        <v-card
          v-for="result in resultsCategories"
          :key="result.contents.info.id"
          class="search-result-card"
        >

          <v-card-title class="flex-column align-baseline">
            <router-link
              openInNewTab
              class="search-results__result-title"
              :to="`/haskell/${result.contents.info.id}`"
            >
              <h2 class="my-0">{{ result.contents.info.title }}</h2>
            </router-link>

            <div class="search-results__result-parent-wrap">in <span class="search-results__result-parent-title">{{result.contents.info.group}}</span></div>
          </v-card-title>
          <v-card-text
            v-if="result.contents.description"
            v-html="result.contents.description.html"
          />

        </v-card>

      </template>

      <template v-if="resultsItems && resultsItems.length">
        <h1 class="mt-10 mb-6"> Items </h1>

        <v-card
          v-for="result in resultsItems"
          :key="result.contents.info.id"
          class="search-result-card"
        >

          <v-card-title class="flex-column align-baseline">
            <router-link
              openInNewTab
              class="search-results__result-title"
              :to="`/haskell/${result.contents.category.id}#item-${result.contents.info.id}`"
            >
              <h2 class="my-0">{{ result.contents.info.name }}</h2>
            </router-link>

            <div class="search-results__result-parent-wrap"
            >in <router-link :to="`/haskell/${result.contents.category.id}`" class="search-results__result-parent-title"
              >{{result.contents.category.title}}</router-link>
            </div>
          </v-card-title>
          <v-card-text
            v-if="result.contents.ecosystem"
            v-html="result.contents.ecosystem.html"
          />

        </v-card>
      </template>

    </template>

    <div
      v-else
      class="text-center display-1"
    >
      Nothing found
    </div>
  </v-container>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Watch } from 'vue-property-decorator'

const resultTagCodes = {
  category: 'Category',
  item: 'Item'
}

@Component
export default class SearchResults extends Vue {
  get query () {
    return this.$store.state.wiki.searchQuery
  }

  get results () {
    return this.$store.state.wiki.searchResults
  }

  get resultsCategories () {
    return this.results.filter(x => x.tag === resultTagCodes.category)
  }

  get resultsItems () {
    return this.results.filter(x => x.tag === resultTagCodes.item)
  }

  @Watch('query')
  async search () {
    await this.$store.dispatch('wiki/search', this.query)
  }

  async serverPrefetch () {
    await this.search()
  }
}
</script>

<style lang="postcss" scoped>
@media only screen and (min-width: 1264px) {
  .search-result-container {
    max-width: 900px;
  }
}
.search-result-card {
  .v-card__text {
    color: #000;
  }

  &:not(:last-child) {
    margin-bottom: 1.5em;
  }
}
.search-results__result-title {
  word-break: break-word;
}
.search-results__result-parent-wrap {
  font-size: 1rem;
}
.search-results__result-parent-title {
  font-weight: 600;
  padding-left: 0;
  line-height: 1;
  max-width: 100%;
  overflow: visible;
  word-break: break-all;
}
.search-result-group-name {
  color: #797979;
}
</style>
