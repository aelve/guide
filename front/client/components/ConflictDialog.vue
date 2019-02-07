<template>
  <v-dialog
    :value="value"
    @input="close"
    max-width="99vw"
  >
    <slot slot="activator" />

    <div class="conflict-box">
      <div class="conflict-item">
        <p class="title mb-2">Your version</p>
        <v-card 
          color="#fdd"
          class="conflict-content"
        >
          <v-card-text>{{modified}}</v-card-text>
        </v-card>
        <v-btn 
          depressed 
          small
          @click="saveUserVersion"
        >
          Submit this version, disregard changes on server
        </v-btn>
      </div>
      <div class="conflict-item">
        <p class="title mb-2">Version on server</p>
        <v-card 
          color="#cfc"
          class="conflict-content"
        >
          <v-card-text>{{serverModified}}</v-card-text>
        </v-card>
        <v-btn 
          depressed 
          small
          @click="saveServerVersion"
        >
          Accept this version, disregard my changes
        </v-btn>
      </div>
      <div class="conflict-item">
        <p class="title mb-2">Merged version</p>
        <markdown-editor
          class="mb-2"
          toolbar
          :value="merged"
          @save="saveMerged"
          @cancel="close"
        />
      </div>
    </div>
  </v-dialog>
</template>

<script lang="ts">
import { Vue, Component, Prop, Watch } from 'vue-property-decorator';
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

  saveUserVersion () {
    this.$emit('saveDescription', this.modified )
    this.close()
  }

  saveServerVersion () {
    this.$emit('saveDescription', this.serverModified)
    this.close()
  }

  saveMerged (newVal: string) {
    this.$emit('saveDescription', this.serverModified)
    this.close()
  }

  close ():void {
    this.$emit('input', false)
  }
}
</script>

<style scoped>
  .conflict-box {
    display: flex;
    background: #fff;
    padding: 20px;
    justify-content: space-between;
  }
  .conflict-content {
    flex: 1;
    margin-bottom: 16px;
    white-space: pre-wrap;
  }
  .conflict-item {
    width: 32%;
    display: flex;
    flex-flow: column;
  }

  @media screen and (max-width: 1200px) {
    .conflict-box {
      flex-wrap: wrap;
    }

    .conflict-item {
      width: 49%;
    }

    .conflict-item:nth-last-child(1) {
      width: 98%;
    }
  }

  @media screen and (max-width: 768px) {
    .conflict-box {
      flex-flow: column;
    }

    .conflict-item {
      width: 100%;
    }
  }
</style>

