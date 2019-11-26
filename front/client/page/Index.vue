<template>
  <categories />
</template>

<script lang="ts">
import Vue from 'vue'
import Categories from 'client/components/Categories.vue'
import Component from 'vue-class-component'

@Component({
  components: {
    Categories
  }
})
export default class Index extends Vue {}
</script>
