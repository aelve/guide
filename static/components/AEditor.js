////////////////////////////////////////////////////////////////////////////
// A multiline text editor with "Submit" and "Cancel" buttons.
//
// Available keybindings:
//   * Ctrl+Enter    Submit
//   * Escape        Cancel
//
// Events:
//   * submit-edit(String)        Some text should be saved
//   * cancel-edit                The edit has been cancelled
//
////////////////////////////////////////////////////////////////////////////

// TODO: check that it's okay that editors have been wrapped into <div>

// TODO: I can recall 'section' being removed from item-info, maybe that's important

Vue.component('AEditor', {
    props: {
        // Text to populate the editor with
        initContent: {type: String, default: ""},
        // Instruction for the user
        instruction: {type: String, required: true},
        // How many rows the editor should have
        rows: {type: Number, required: true},
        // Whether editor content should be reset on cancel
        resetOnCancel: {type: Boolean, default: true},
    },

    data: function() { return {
        content: this.initContent,
    }},

    methods: {
        onSubmitEdit() {
            this.$emit('submit-edit', this.content); },
        onCancelEdit() {
            if (this.resetOnCancel) this.content = this.initContent;
            this.$emit('cancel-edit'); },
    },

    // TODO: another messy template; also, can use <button> instead of <input>;
    // also, "editor" should be changed to "editor-area" or something, and we
    // should provide a method to focus the editor area
    //
    // Autocomplete has to be turned off because of this issue:
    // http://stackoverflow.com/q/8311455
    //
    // We also have to check for keycode 10 to handle Ctrl+Enter thanks to this
    // Chrome bug: https://bugs.chromium.org/p/chromium/issues/detail?id=79407
    template: `
<div>
    <textarea
        v-model="content"
        autocomplete="off"
        class="big fullwidth editor"
        :rows="rows"
        @keydown.ctrl.enter.prevent="onSubmitEdit" @keydown.ctrl.10.prevent="onSubmitEdit"
        @keydown.meta.enter.prevent="onSubmitEdit" @keydown.meta.10.prevent="onSubmitEdit"
        @keydown.esc.prevent="onCancelEdit"
    ></textarea>

    <input type="button" value="Save" class="save" @click="onSubmitEdit">
    <span style="margin-left:6px"></span>
    <input type="button" value="Cancel" class="cancel" @click="onCancelEdit">
    <span style="margin-left:6px"></span>
    <span class="edit-field-instruction">{{ instruction }}</span>
    <a href="/markdown" target="_blank">
        <img class="markdown-supported"
             src="/markdown.svg" alt="Markdown supported">
    </a>
</div>
`
});

////////////////////////////////////////////////////////////////////////////
// A single-line editor with an optional "Cancel" button.
//
// Available keybindings:
//   * Enter         Submit
//   * Escape        Cancel
//
// Events:
//   * submit-edit(String)        Some text should be saved
//   * cancel-edit                The edit has been cancelled
//
////////////////////////////////////////////////////////////////////////////

Vue.component('AEditorMini', {
    props: {
        // Text to populate the editor with
        initContent: {type: String, default: ""},
        // Instruction for the user
        instruction: {type: String, required: true},
        // How many rows the editor should have
        rows: {type: Number, required: true},
        // Whether editor content should be reset on cancel
        resetOnCancel: {type: Boolean, default: true},
        // Whether to allow cancellation
        allowCancel: {type: Boolean, default: true},
        // Placeholder
        placeholder: {type: String, default: ""},
    },

    data: function() { return {
        content: this.initContent,
    }},

    methods: {
        onSubmitEdit: function() {
            this.$emit('submit-edit', this.content);
            // TODO: we should have a separate method for that and only call it
            // if the event handler has succeeded (outside of this component)
            this.content = ""; },
        onCancelEdit: function() {
            if (this.allowCancel) {
                if (this.resetOnCancel) this.content = this.initContent;
                this.$emit('cancel-edit'); } },
    },

    template: `
<div>
    <textarea
        v-model="content"
        autocomplete="off"
        class="fullwidth"
        :rows="rows"
        :placeholder="placeholder"
        @keydown.enter.prevent="onSubmitEdit"
        @keydown.esc.prevent="onCancelEdit"
    ></textarea>

    <br>
    <span v-if="allowCancel" class="text-button">
        <a href="#" @click.prevent="onCancelEdit">cancel</a>
    </span>
    <span style="float:right">
        <span class="edit-field-instruction">{{ instruction }}</span>
        <a href="/markdown" target="_blank">
            <img class="markdown-supported"
                 src="/markdown.svg" alt="Markdown supported">
        </a>
    </span>
</div>
`
});
