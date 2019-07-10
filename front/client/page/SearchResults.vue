<template>
  <v-container class="search-result-container">
    <template v-if="results && results.length">
      <v-card
        v-for="(result, index) in results"
        :key="index"
        class="search-result-card"
      >

        <v-card-title v-if="result.tag === 'Category'">
          <span class="search-result-title">
            <span class="search-result-group-name">
              {{ result.contents.info.group }}
            </span>
              »
            <router-link
              openInNewTab
              :to="`/haskell/${result.contents.info.id}`"
            >{{ result.contents.info.title }}</router-link>
          </span>
        </v-card-title>

        <v-card-title v-else>
          <span class="search-result-title">
            <router-link
              openInNewTab
              :to="`/haskell/${result.contents.category.id}`"
            >{{ result.contents.category.title }}</router-link>
              »
            <span>
              <router-link
                openInNewTab
                :to="`/haskell/${result.contents.category.id}#item-${result.contents.info.id}`"
              >{{ result.contents.info.name }}</router-link>'s ecosystem
            </span>
          </span>
        </v-card-title>

        <v-card-text
          v-if="result.tag === 'Category' && result.contents.description"
          v-html="result.contents.description.html"
        />
        <v-card-text
          v-if="result.tag === 'Item' && result.contents.ecosystem"
          v-html="result.contents.ecosystem.html"
        />
      </v-card>
    </template>

    <div
      v-else
      class="text-md-center display-1"
    >
      Nothing found
    </div>
  </v-container>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import ALink from 'client/components/ALink.vue'

@Component({
  components: {
    ALink
  }
})
export default class SearchResults extends Vue {
  @Prop(String) query!: string

  get results () {
    return this.$store.state.wiki.searchResults
  }

  beforeMount () {
    // This watch should be added in beforeMount (only on client) hook because setDocumentTitle function uses DOM api which is undefined on server
    this.$watch('query', this.setDocumentTitle, { immediate: true })
  }

  mounted () {
    this.$store.commit('wiki/setSearchInput', this.query)
  }

  async serverPrefetch () {
    await this.search()
  }

  @Watch('query')
  async search () {
    await this.$store.dispatch('wiki/search', this.query)
  }

  setDocumentTitle (query) {
    document.title = `${query} – Search results – Aelve Guide`
  }
}
</script>

<style scoped>
@media only screen and (min-width: 1264px) {
  .search-result-container {
    max-width: 900px;
  }
}
.search-result-card:not(:last-child) {
  margin-bottom: 1.5em;
}
.search-result-title {
  font-size: 22px;
  font-weight: 600;
  line-height: 28px;
}
.search-result-group-name {
  color: #797979;
}
</style>
