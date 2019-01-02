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
          <v-card-text>{{serverModified}}</v-card-text>
        </v-card>
        <v-btn 
          depressed 
          small
          @click="$emit('saveDescription', { original: serverModified, modified: serverModified}); close();"
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
          <v-card-text>{{modified}}</v-card-text>
        </v-card>
        <v-btn 
          depressed 
          small
          @click="$emit('saveDescription', { original: serverModified, modified: modified}); close();"
        >
          Accept this version, disregard my changes
        </v-btn>
      </div>
      <div class="conflict-item">
        <p class="title mb-2">Merge version</p>
        <v-textarea
          :value="merged"
          :v-model="showMergedDescription"
          auto-grow
          solo
        />
        <v-btn 
          depressed 
          small
          @click="$emit('saveDescription', { original: serverModified, modified: merged}); close();"
        >
          Submit the merged version
        </v-btn>
      </div>
    </div>
  </v-dialog>
</template>

<script lang="ts">
import { Vue, Component, Prop, Watch } from 'vue-property-decorator';

@Component
export default class ConflictDialog extends Vue {
  @Prop(Boolean) value!: boolean
  @Prop(String) serverModified!: string
  @Prop(String) modified!: string
  @Prop(String) merged!: string

  mergedDescription: string = ''
  
  showMergedDescription () {
    return this.mergedDescription = this.merged
  }

  close () {
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
  }
  .conflict-item {
    width: 32%;
    display: flex;
    flex-flow: column;
  }

  .conflict-item >>> textarea {
    padding: 16px 16px 16px 4px;
    font-size: 14px;
    margin: 0 !important;
  }
/* Костыль под textarea есть непонятный div - костыль для того чтобы не было разницы в height между блоками */
  .v-input__slot {
    padding-bottom: 8px;
  }

  .conflict-item >>> .v-text-field__details {
    height: 0!important;
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

