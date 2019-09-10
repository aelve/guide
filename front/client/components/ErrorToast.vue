<template>
  <v-snackbar
    top
    right
    multi-line
    vertical
    color="#db3737"
    class="error-toast"
    :value="value"
    :timeout="0"
    @input="onInput"
    @blur="startTimeout"
    @focus="clearTimeout"
    @mouseenter="clearTimeout"
    @mouseleave="startTimeout"
  >
    <!--
      workaround of this bug https://github.com/vuetifyjs/vuetify/issues/7310
      TODO when fixed
    -->
    <div class="v-menu__content--active" style="display:none; z-index:1000;" />

    <div class="error-toast__header">
      <v-icon class="error-toast__header__icon" color="#fff" size="16" v-text="'$vuetify.icons.exclamation-triangle'" />
      <span class="error-toast__header__msg">{{message}}</span>
      <v-btn
        icon
        color="hsla(0,0%,100%,.7)"
        class="error-toast__header__close-btn"
        aria-label="Close"
        @click="close"
      >
        <v-icon size="16" v-text="'$vuetify.icons.times'" />
      </v-btn>
    </div>

    <template v-if="details && typeof details === 'object'">
      <v-btn
        class="error-toast__details-btn"
        text color="hsla(0,0%,100%,.7)"
        @click="toggleDetails"
      >
        Details
      </v-btn>

      <v-slide-y-transition hide-on-leave>
        <div
          v-show="isDetailsVisible"
          class="error-toast__details"
        >

          <div v-if="details.path != null">
             <span class="font-weight-bold">path</span>:
             <div class="error-toast__details__text">{{ details.path }}</div>
          </div>
          <div v-if="details.responseCode != null">
             <span class="font-weight-bold">response code</span>:
             <div class="error-toast__details__text">{{ details.responseCode }}</div>
          </div>
          <div v-if="details.message != null">
             <span class="font-weight-bold">message</span>:
             <div class="error-toast__details__text">"{{ details.message }}"</div>
          </div>

        </div>
      </v-slide-y-transition>
    </template>
  </v-snackbar>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'

const defaultTimeout = 2700

@Component
export default class ErrorToast extends Vue {
  @Prop({
    type: String,
    required: true
  }) message
  @Prop(Object) details

  value = false
  isDetailsVisible = false
  timeout = null

  mounted () {
    this.value = true
    this.$nextTick(() => this.startTimeout())
  }

  // TODO set timeout to 0 if hovered and reset on blur
  onInput (value) {
    if (!value) {
      this.close()
    }
  }

  startTimeout () {
    this.timeout = setTimeout(() => this.close(), defaultTimeout)
  }

  clearTimeout () {
    if (!this.timeout) {
      return
    }
    clearTimeout(this.timeout)
    this.timeout = null
  }

  close () {
    this.value = false
    this.clearTimeout()
    this.$emit('closed')
  }

  toggleDetails () {
    this.isDetailsVisible = !this.isDetailsVisible
  }
}
</script>

<style lang="postcss" scoped>
.error-toast.v-snack--vertical {
  left: unset;
  margin: 0;

  >>> .v-snack__wrapper {
    min-width: unset;
  }

  >>> .v-snack__content {
    padding: 6px 14px;
    word-break: break-word;
  }
}
.error-toast__header {
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
}
.error-toast__header__icon {
  margin-top: 6px;
}
.error-toast__header__msg {
  padding: 3px 8px 8px 8px;
  font-size: 16px;
  font-weight: bold;
}
.error-toast__details__text {
  font-style: italic;
  margin-left: 8px;
}
.v-snack--vertical
  .v-snack__content
  .v-btn.v-btn.error-toast__header__close-btn {
  align-self: auto;
  justify-self: auto;
  margin-top: 0;
  padding: 8px;

  &:hover {
    background-color: hsla(0, 0%, 100%, 0.15);
  }
}
.v-snack--top {
  top: 72px;
}
.error-toast__details-btn {
  text-transform: none;
  align-self: flex-start !important;
  margin-top: 0 !important;
  padding: 4px !important;
}
.error-toast__details {
  margin-left: 8px;
  max-height: 250px;
  overflow: auto;
}
>>> .v-snack__wrapper {
  max-width: 350px;

  @media screen and (max-width: 768px) {
    max-width: 250px;
  }

  @media screen and (max-width: 425px) {
    max-width: 175px;
  }
}
</style>