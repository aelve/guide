////////////////////////////////////////////////////////////////////////////
// Item description (also called "Summary")
//
// Provides a way to edit the description. If the content changed on the backend
// in the meantime, opens a dialog and offers to resolve potential merge
// conflicts.
////////////////////////////////////////////////////////////////////////////

Vue.component('CategoryItemDescription', {
    props: {
        /** The ID of the item that the description belongs to */
        itemId: {type: String, required: true},
        /** Description itself, contains `text` and `html` */
        initContent: {type: Object, required: true},
    },

    data: function() { return {
        /** 
         * `editing` signals whether we're in editing mode
         * */
        editing: false,
        /** 
         * `unsaved` signals whether the editor might have unsaved changes. If
         * this is the case, we hide it instead of destroying it. When the user
         * clicks "Cancel", their changes get lost; when they click the pencil
         * button, they remain. 
         * */
        unsaved: false,
        /**
         * `startingContent` is the "starting point" of editing. When we submit,
         * the backend does a three-way diff between the description as we've
         * last seen it (i.e. `startingContent`), our proposed edited
         * description, and the one that the backend currently has.
         * */
        startingContent: initContent,
    }},

    computed: {
        /** DOM identifier for our item */
        itemNode: function() {
            return "item-" + this.itemId },
    },

    methods: {
        /**
         * Submit description to the backend and, if it changed on the backend
         * in the meantime, show a merge resolution popup. If the content
         * changes *again* while the user resolves the merge, we'll show another
         * popup, etc.
         *
         * TODO: it would be nice to replace `startingContent` when we submit
         * the merged version, but we don't get rendered Markdown from the
         * backend, so we can't do that.
         * */
        submitDescription(startingContentText, editedContentText) {
            $.post({
                url: `/haskell/set/item/${this.itemId}/description`,
                data: {
                    original: startingContentText,
                    content: editedContentText },
                success: (data) => {
                    $.magnificPopup.close();
                    this.editing = false;
                    this.unsaved = false;
                    this.startingContent = data; },
                statusCode: {
                    409: (xhr, st, err) => {
                        /** Current item description on the backend */
                        let backendText = xhr.responseJSON["modified"];
                        /** Backend's proposed merge (the diff between our
                         * 'startingContentText' and 'editedContentText, applied
                         * to 'backendText') */
                        let proposedMerge = xhr.responseJSON["merged"];
                        showDiffPopup(startingContentText, backendText, proposedMerge,
                            (ourMerge) => {
                                // Now the user looked at the `backendText` and
                                // (hopefully) applied their edits to it, so
                                // `backendText` becomes the new starting point
                                this.submitDescription(backendText, ourMerge) }); } },
            });          
        },
    },

    // TODO: we don't need 'section' anymore, I think. Also, there's a bit of
    // duplication in this template.
    //
    // TODO: check that when an editor opens, it gets focus (everywhere we use
    // an editor)
    template: `
<div class="item-description">
    <div v-if="!editing" class="section normal">
        <strong>Summary</strong>
        <span style="margin-left:0.5em"></span>
        <a href="#" class="small-control edit-item-description"
            @click.prevent="editing=true; unsaved=true;">
            <img src="/pencil.svg" title="edit summary"></a>
        <div class="notes-like">
            <template v-if="content.text != ''" v-html="content.html"></template>
            <p v-else>write something here!</p>
        </div>
    </div>

    <div v-if="editing || unsaved" v-show="editing" class="section editing">
        <strong>Summary</strong>
        <span style="margin-left:0.5em"></span>
        <a href="#" class="small-control edit-item-description"
            @click.prevent="editing=false; unsaved=true;">
            <img src="/pencil.svg" title="hide the editor"></a>
        <AEditor
            :init-content="content.text"
            :instruction="'or press Ctrl+Enter to save'"
            @submit-edit="(e) => submitDescription(startingContent.text, e)"
            @cancel-edit="editing=false; unsaved=false;"
        />
    </div>
</div>
`,
});

// rows=10 should go somewhere
