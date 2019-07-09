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
      const isLineInCodeblock = line.styles.some(x => typeof x === 'string' && x.includes('comment'))
      // Highlightens full line inside code block, since codeMirror itself highlights only symbols
      if (isLineInCodeblock) {
        element.style.background = 'rgba(0, 0, 0, 0.05)'
        element.style.fontFamily = 'monospace, monospace'
      }
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

<style lang="postcss" scoped>
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
  font-size: 16px;

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

  .cm-comment {
    background: unset;
  }
}
>>> .v-toolbar__content {
  padding-left: 0;
}
.markdown-editor-save-tip {
  font-size: 11px;
}
</style>