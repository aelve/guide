<template>
  <v-container class="search-result-container">
    <v-card
      v-if="results && results.length"
      v-for="(result, index) in results"
      :key="index"
      class="search-result-card"
    >

      <v-card-title
        v-if="result.tag === 'Category'"
      >
        <span class="search-result-title">
          <span class="search-result-group-name">
            {{ result.contents.info.group }}
          </span>
            »
          <a-link
            openInNewTab
            :url="`http://aelve.com:4801/haskell/${result.contents.info.uid}`"
          >
            {{ result.contents.info.title }}
          </a-link>
        </span>
      </v-card-title>

      <v-card-title
        v-else
      >
        <span class="search-result-title">
          <a-link
            openInNewTab
            :url="`http://aelve.com:4801/haskell/${result.contents.category.uid}`"
          >
            {{ result.contents.category.title }}
          </a-link>
            »
          <span>
            <a-link
              openInNewTab
              :url="`http://aelve.com:4801/haskell/${result.contents.category.uid}#item-${result.contents.info.uid}`"
            >
            <!-- Do not format next line to separate lines cause it adds extra space after </a-link>. -->
            {{ result.contents.info.name }}</a-link>'s ecosystem
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

    <div
      v-if="!results || !results.length"
      class="text-md-center display-1"
    >
      Nothing found
    </div>
  </v-container>
</template>

<script lang="ts">
import { Vue, Component, Prop, Watch } from 'vue-property-decorator'
import ALink from 'client/components/ALink.vue'

@Component({
  components: {
    ALink
  }
})
export default class SearchResults extends Vue {
  @Prop(String) query!: string

  mounted () {
    this.$store.commit('wiki/setSearchInput', this.query)
  }

  async asyncData () {
    await this.$store.dispatch('wiki/search', this.query)
  }

  get results () {
    return this.$store.state.wiki.searchResults
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
