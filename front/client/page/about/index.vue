<template lang="pug">
  div.about-page
    h1 About page.
    p(v-if="isLoading") Loading...
    p(v-else) {{oldSaying}}

    div
      Button(@click="changeOldSaying") Change old saying

    div
      router-link(to="/") Index
      br
      router-link(to="/about/who") About who?
</template>

<script lang="ts" src="./index.ts"></script>
