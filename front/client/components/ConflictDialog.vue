<template>
  <v-dialog
    persistent
    :value="value"
  >

    <template
      v-if="$slots.activator"
      v-slot:activator="{ on }"
    >
      <slot
        slot="activator"
        v-on="on"
      />
    </template>

    <div class="conflict-box">
      <div class="conflict-item">
        <h2 class="mt-0">Your version</h2>
        <v-card
          color="#fdd"
          class="conflict-content"
        >
          <v-card-text>{{modified}}</v-card-text>
        </v-card>
        <v-btn
          class="conflict-dialog-btn"
          title="Submit this version, disregard changes on server"
          @click="save(modified)"
        >
          Submit this version, disregard changes on server
        </v-btn>
      </div>
      <div class="conflict-item">
        <h2 class="mt-0">Version on server</h2>
        <v-card 
          color="#cfc"
          class="conflict-content"
        >
          <v-card-text>{{serverModified}}</v-card-text>
        </v-card>
        <v-btn
          class="conflict-dialog-btn"
          title="Submit this version, disregard my changes"
          @click="save(serverModified)"
        >
          Accept this version, disregard my changes
        </v-btn>
      </div>
      <div class="conflict-item">
        <h2 class="mt-0">Merged version</h2>
        <markdown-editor
          toolbar
          class="conflict-content_markdown"
          height="auto"
          v-model="mergedEdit"
          :autofocus="false"
          :bottomToolbar="false"
          @save="save"
        />
        <v-btn
          class="conflict-dialog-btn"
          title="Submit merged version"
          @click="save(mergedEdit)"
        >
          Submit the merged version
        </v-btn>
      </div>
    </div>
  </v-dialog>
</template>

<script lang="ts">
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'
import MarkdownEditor from 'client/components/MarkdownEditor.vue'

@Component({
  components: {
    MarkdownEditor,
  }
})
export default class ConflictDialog extends Vue {
  @Prop(Boolean) value!: boolean
  @Prop(String) serverModified!: string
  @Prop(String) modified!: string
  @Prop(String) merged!: string

  mergedEdit = this.merged

  @Watch('merged')
  onMergedChange (newVal) {
    this.mergedEdit = newVal
  }

  save (newValue: string) {
    this.$emit('save', newValue)
  }

}
</script>

<style lang="postcss" scoped>
>>> .v-dialog {
  overflow-x: hidden;
}
.conflict-box {
  display: flex;
  background: #fff;
  padding: 20px;
  max-height: 90vh;

  > *:not(:last-child) {
    margin-right: 1.65rem;
  }
}
.conflict-item {
  display: flex;
  max-height: 100%;
  overflow: hidden;
  align-items: center;
  flex-direction: column;
  flex: 1;

  > h2 {
    text-align: center;
  }
}
.conflict-content,
.conflict-content_markdown {
  display: flex;
  flex-direction: column;
  flex: 1;
  width: 100%;
  margin-bottom: 16px;
  white-space: pre-wrap;
  overflow: auto;

  >>> {
    .CodeMirror {
      flex: 1;
    }
    .v-card__text {
      word-break: break-word;
      font-size: 1rem;
      line-height: inherit;
      letter-spacing: normal;
      color: #000;
    }
  }
}
.conflict-dialog-btn {
  display: block;
  margin: 0;
  min-height: 60px;
  min-width: 220px;
  max-height: 60px;
  max-width: 220px;
  padding: 6px 16px;
  font-weight: bold;
  font-size: 0.7rem;

  >>> .v-btn__content {
    width: unset;
    white-space: normal;
  }
}
/* Without this styling codemirror input area scrolling breaks */
.conflict-content_markdown >>> .CodeMirror-scroll {
  min-height: 0 !important;
}
@media screen and (max-width: 900px) {
  .conflict-box {
    flex-direction: column;
    max-height: unset;

    > *:not(:last-child) {
      margin-right: 0;
      margin-bottom: 1.65rem;
    }
  }

  /* For every content area to be the same */
  .conflict-content,
  .conflict-content_markdown {
    max-height: 350px;
    /* Without height setting codemirror input area is not scrollable */
    height: 350px;
  }
}
</style>

