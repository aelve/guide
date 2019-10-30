<template>
  <router-link
    class="categories__category-link"
    data-testid="Categories-CategoryTitle"
    :to="`/haskell/${getCategoryUrl(category)}`"
  >
    {{ category.title }}
  </router-link>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop } from 'vue-property-decorator'
import getCategoryUrl from 'client/helpers/getCategoryUrl'

@Component
export default class CategoriesCategoryLink extends Vue {
  @Prop(Object) category: object

  getCategoryUrl = getCategoryUrl
}
</script>

<style scoped>
.categories__category-link {
  font-size: 0.9rem;
  font-weight: 600;
  margin-top: 5px;
  line-height: 1.2;
}
</style>