<template>
<!-- Wrapper div is required for correct destroying of component
     cause easyMDE adds new html elements next to textarea -->
  <div
    class="elevation-2"
    @keydown.capture.enter="onEnterDown"
    @keydown.ctrl.enter="save"
    @keydown.esc="cancel"
    v-show="editor && isReady"
  >
    <textarea ref="editor" />

    <v-toolbar
      flat
      height="30"
      color="#e5e5e5"
      class="pa-2 markdown-editor-bottom-toolbar"
      v-show="editor"
    >
      <v-toolbar-items>
        <v-btn
          small
          title="Save"
          class="mr-2 text-transform-none"
          @click="save"
        >
          Save
        </v-btn>
        <v-btn
          small
          title="Cancel"
          class="text-transform-none"
          @click="cancel"
        >
          Cancel
        </v-btn>
      </v-toolbar-items>
      <span class="markdown-editor-save-tip ml-1">
        {{ saveTip }}
      </span>
    </v-toolbar>
  </div>
</template>

<script lang="ts">
import 'easymde/dist/easymde.min.css'
import Vue from 'vue'
import Component from 'vue-class-component'
import { Prop, Watch } from 'vue-property-decorator'

@Component
export default class MarkdownEditor extends Vue {
  @Prop({
    type: String,
    default: ''
  }) value: string
  @Prop({
    type: Number,
    default: 300
  }) height: number
  @Prop(Boolean) toolbar: boolean
  @Prop(Boolean) saveOnEnter: boolean

  editor: object = null
  isReady: boolean = false

  get saveTip () {
    return `press${this.saveOnEnter ? ' Enter or' : ''} Ctrl+Enter to save`
  }

  @Watch('value')
  onValueChange (newVal: string): void {
    if (!this.editor || this.editor.value() === newVal) {
      return
    }
    this.editor.value(newVal)
  }

  async beforeMount () {
    await this.createEditorInstance()
    this.setInputAreaHeight()
    this.isReady = true
    this.focusInputArea()
  }

  async createEditorInstance () {
    const EasyMDE = (await import('easymde')).default
    this.editor = new EasyMDE({
      element: this.$refs.editor,
      autofocus: true,
      initialValue: this.value,
      spellChecker: false,
      status: false,
      minHeight: `${this.height}px`,
      toolbar: this.toolbar
        ? [
          'bold',
          'italic',
          'strikethrough',
          'code',
          'quote',
          'heading',
          'heading-smaller',
          'heading-bigger',
          '|',
          'unordered-list',
          'ordered-list',
          '|',
          'link',
          'image',
          'horizontal-rule',
          '|',
          'preview',
          'side-by-side',
          'fullscreen',
          {
            name: 'guide',
            action () {
              window.open('https://commonmark.org/help/', '_blank')
            },
            className: 'fa fa-question-circle',
            title: 'Markdown Guide',
          },
        ]
        : false,
      renderingConfig: {
        markedOptions: {
          gfm: false
        }
      },
      indentWithTabs: false
    })
    this.editor.codemirror.on('change', () => {
      this.$emit('input', this.editor.value())
    })
  }

  setInputAreaHeight () {
    const inputAreaEl = this.$el.querySelector('.CodeMirror') as HTMLElement
    if (!inputAreaEl) {
      return
    }
    inputAreaEl.style.height = `${this.height}px`
  }

  focusInputArea () {
    // this function is triggered right after isReady set to true
    // isReady controls v-show of entire markup of component
    // nextTick is used cause html needs to be rendered after v-show triggered so focus will work
    this.$nextTick(() => document.querySelector('.CodeMirror textarea').focus())
  }

  onEnterDown (event: KeyboardEvent) {
    if (this.saveOnEnter) {
      event.preventDefault()
      this.save()
    }
  }

  save () {
    this.$emit('save', this.editor.value())
  }

  cancel () {
    this.$emit('cancel')
  }
}
</script>

<style scoped>
>>> .editor-toolbar,
>>> .CodeMirror {
  border: none;
  border-radius: 0;
  border-bottom: 1px solid #bbb;
}
>>> .CodeMirror {
  /* Fixes cutting of bottom edge of input
     https://github.com/sparksuite/simplemde-markdown-editor/issues/619
  */
  box-sizing: content-box;
}
>>> .CodeMirror {
  font-size: 12px;
}
>>> .v-toolbar__content {
  padding-left: 0;
}
.markdown-editor-save-tip {
  font-size: 11px;
}
</style>