<template>
<!-- Wrapper div is required for correct destroying of component
     cause easyMDE adds new html elements next to textarea -->
  <div
    class="elevation-2"
    :class="{
      'markdown-editor_has-bottom-toolbar': bottomToolbar,
      'markdown-editor_has-top-toolbar': toolbar
    }"
    @keydown.exact.capture.enter="onEnterDown"
    @keydown.exact.ctrl.enter="onCtrlEnterDown"
    @keydown.exact.meta.enter="onCtrlEnterDown"
    @keydown.exact.esc="onEsc"
    v-show="editor && isReady"
  >
    <textarea
      data-testid="MarkdownEditor-OriginalTextarea"
      ref="editor"
    />

    <v-toolbar
      flat
      height="auto"
      color="#e5e5e5"
      class="markdown-editor__bottom-toolbar"
      v-if="bottomToolbar"
      v-show="editor"
    >
      <span class="markdown-editor-save-tip">
        {{ saveTip }}
      </span>
      <v-toolbar-items>
        <v-btn
          text
          class="mr-2"
          aria-label="Cancel"
          @click="cancel"
        >
          Cancel
        </v-btn>
        <v-btn
          data-testid="MarkdownEditor-SaveBtn"
          color="info"
          aria-label="Save"
          @click="save"
        >
          Save
        </v-btn>
      </v-toolbar-items>
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
    type: [Number, String],
    default: 300
  }) height: number | string
  @Prop(Boolean) toolbar: boolean
  @Prop(Boolean) saveOnEnter: boolean
  @Prop({
    type: Boolean,
    default: true
  }) saveOnCtrlEnter: boolean
  @Prop({
    type: Boolean,
    default: true
  }) autofocus: boolean
  @Prop({
    type: Boolean,
    default: true
  }) bottomToolbar: boolean

  editor: object = null
  isReady: boolean = false

  get saveTip () {
    return `press ${this.saveOnEnter ? 'Enter' : 'Ctrl/Cmd + Enter'} to save`
  }

  get heightValue () {
    return Number(this.height)
      ? `${this.height}px`
      : this.height
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
    this.isReady = true
    this.$nextTick(() => this.setInputAreaHeight())
    this.focusInputArea()
  }

  async createEditorInstance () {
    const EasyMDE = (await import('easymde')).default
    this.editor = new EasyMDE({
      element: this.$refs.editor,
      forceSync: true,
      autofocus: true,
      initialValue: this.value,
      spellChecker: false,
      status: false,
      minHeight: this.heightValue,
      toolbar: this.toolbar
        ? [
          'bold',
          'italic',
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
      shortcuts: {
        toggleBold: "Ctrl-B",
        toggleItalic: "Ctrl-I",
        toggleCodeBlock: "Ctrl-Alt-C",
        toggleBlockquote: "Ctrl-'",
        toggleHeadingSmaller: "Ctrl-H",
        toggleHeadingBigger: "Shift-Ctrl-H",
        toggleUnorderedList: "Ctrl-L",
        toggleOrderedList: "Ctrl-Alt-L",
        drawLink: "Ctrl-K",
        drawImage: "Ctrl-Alt-I",
        toggleFullScreen: "F11",
        togglePreview: null,
        cleanBlock: null,
        toggleSideBySide: null
      },
      renderingConfig: {
        markedOptions: {
          gfm: false
        }
      },
      parsingConfig: {
        strikethrough: false
      },
      indentWithTabs: false
    })
    this.editor.codemirror.on('change', () => {
      this.$emit('input', this.editor.value())
    })
    this.editor.codemirror.on('renderLine', (codeMirror, line, element) => {
      if (!line.styles) {
        return
      }
      const isLineInCodeblock = line.styles
        .some(x => typeof x === 'string' && x.includes('comment'))
      // Highlightens full line inside code block, since codeMirror itself highlights only symbols
      if (isLineInCodeblock) {
        element.style.fontFamily = 'monospace, monospace'
        element.style.fontSize = '85%'
      }
    })
  }

  setInputAreaHeight () {
    const inputAreaEl = this.$el.querySelector('.CodeMirror') as HTMLElement
    if (!inputAreaEl) {
      return
    }
    inputAreaEl.style.height = this.heightValue
  }

  focusInputArea () {
    if (!this.autofocus) {
      return
    }
    // this function is triggered right after isReady set to true
    // isReady controls v-show of entire markup of component
    // nextTick is used cause html needs to be rendered after v-show triggered so focus will work
    this.$nextTick(() => this.editor.codemirror.focus())
  }

  onEnterDown (event: KeyboardEvent) {
    if (this.saveOnEnter) {
      event.preventDefault()
      this.save()
    }
  }

  onCtrlEnterDown (event) {
    if (this.saveOnCtrlEnter) {
      event.preventDefault()
      this.save()
    }
  }

  save () {
    this.$emit('save', this.editor.value())
  }

  async onEsc () {
    let isConfirmed = true
    if (this.value !== this.editor.value()) {
      isConfirmed = await this._confirm({
        fullText: 'You have unsaved changes. Do you really want to discard them?',
        confirmBtnText: 'Disacrd',
        confirmBtnProps: {
          color: 'error'
        }
      })
    }
    if (isConfirmed) {
      this.cancel()
    }
  }

  cancel () {
    this.$emit('cancel')
  }
}
</script>

<style lang="postcss" scoped>
>>> .editor-toolbar {
  font-size: 0.8rem;

  button {
    display: inline-flex;
    align-items: baseline;
    justify-content: center;

    &:after {
      position: static;
    }
  }
}
>>> .CodeMirror {
  border: 1px solid #bbb;
  border-radius: 0;

  /* Fixes cutting of bottom edge of input
     https://github.com/sparksuite/simplemde-markdown-editor/issues/619
  */
  box-sizing: content-box;
  font-size: 1rem;

  .cm-header-1 {
    font-size: 2rem;
  }
  .cm-header-2 {
    font-size: 1.5rem;
  }
  .cm-header-3 {
    font-size: 1.17rem;
  }
  .cm-header-5 {
    font-size: 0.83rem;
  }
  .cm-header-6 {
    font-size: 0.67rem;
  }
}
.markdown-editor_has-top-toolbar >>> .CodeMirror {
  border-top: 1px solid #ddd;
}
.markdown-editor_has-bottom-toolbar >>> .CodeMirror {
  border-bottom: 1px solid #ddd;
}
.markdown-editor__bottom-toolbar {
  >>> {
    .v-toolbar__content {
      padding: 8px;
      justify-content: flex-end;
    }
    .v-toolbar__items > .v-btn {
      height: 26px !important;
      border-radius: 4px;
    }
  }

  .markdown-editor-save-tip {
    font-size: 11px;
    line-height: 14px;
    margin-right: 5px;

    @media screen and (max-width: 425px) {
      display: none;
    }
  }
}
</style>