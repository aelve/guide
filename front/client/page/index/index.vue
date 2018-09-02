<template lang="pug">
  div.index-page
    h1 Greeting from {{pageName}}!

    div
      p This is your user list:
      ul
        li(v-for="item in userList") Name: {{item.name}}, Age: {{item.age}}

    p You can checkout these page too:
    ul
      li
        router-link(to="/about/me") About
</template>

<script lang="ts" src="./index.ts"></script>
