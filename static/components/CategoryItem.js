////////////////////////////////////////////////////////////////////////////
// A single item on a category page.
//
// For now, each item is a separate Vue.js "app", and stuff like moving
// items up and down is done by modifying DOM with jQuery. As we keep
// rewriting the frontend, jQuery would be replaced by editing the order
// of items in the category object.
////////////////////////////////////////////////////////////////////////////

Vue.component('CategoryItem', {
    props: {
        /** Contents of the item */
        item: {type: Object, required: true},
        /** Item hue (depends on some data in the category, hence not part of
         * the item) */
        hue: {type: Number, required: true},
        /** The category defines some sections to be visible or hidden */
        enabledTraits:    {type: Boolean, required: true},
        enabledEcosystem: {type: Boolean, required: true},
        enabledNotes:     {type: Boolean, required: true},
    },

    data: function() { return {
        /** Whether to show the item info edit form */
        editing: false,
    }},

    computed: {
        /** DOM identifier for our item */
        itemNode: function() {
            return "item-" + this.item.uid },
    },

    methods: {
        /**
         * When the "up"/"down" control is clicked, move the item one position
         * up/down, both on the backend and the frontend.
         */
        onMoveUp() {
            $.post("/haskell/move/item/" + this.item.uid, {direction: "up"})
             .done(() => { moveNodeUp('#' + this.itemNode);
                           fadeIn('#' + this.itemNode); });
        },

        onMoveDown() {
            $.post("/haskell/move/item/" + this.item.uid, {direction: "down"})
             .done(() => { moveNodeDown('#' + this.itemNode);
                           fadeIn('#' + this.itemNode); });
        },

        /**
         * When the "delete" control is clicked, notify the backend and then
         * remove the item entirely.
         */
        onDelete() {
            if (confirm("Confirm deletion?")) {
                $.post("/haskell/delete/item/" + this.item.uid)
                 .done(() => { fadeOutAndRemove('#' + this.itemNode); });
            }
        },
    },

    template: `
<div :id="itemNode" class="item">
    <div class="item-info" :style="{ backgroundColor: darkColor(this.hue) }">
        <CategoryItemInfo v-if="!editing"
            @move-item-up="onMoveUp"
            @move-item-down="onMoveDown"
            @delete-item="onDelete"
            @edit-item-info="editing=true"
        />
        <CategoryItemInfoEdit v-if="editing"
            @cancel-edit="editing=false"/>
    </div>
    <div class="item-body" :style="{ backgroundColor: lightColor(this.hue) }">
        <CategoryItemDescription/> ???
        <CategoryItemTraits    v-if="enabledTraits"/>
        <CategoryItemEcosystem v-if="enabledEcosystem"/>
        <CategoryItemNotes     v-if="enabledNotes"/>
    </div>
</div>
`,
});

////////////////////////////////////////////////////////////////////////////
// Hues used to color items. For each hue (integer) there is a light color
// and a dark color. -1 stands for a colorless item.
//
// The colors were taken from Google's color palette at
// https://www.google.com/design/spec/style/color.html#color-color-palette
// ("100" for dark and "50" for light), except for gray which didn't really
// fit.
////////////////////////////////////////////////////////////////////////////

/**
 * @param {Number} hue    Integer number (or -1 for "colorless")
 * @returns {String}      Color code, e.g. "#D1C4E9"
 */
function darkColor(hue) {
    // deep purple, green, amber, blue,
    // red, brown, teal, lime
    var colors = [
        "#D1C4E9", "#C8E6C9", "#FFECB3", "#BBDEFB",
        "#FFCDD2", "#D7CCC8", "#B2DFDB", "#F0F4C3"];
    if (hue == -1) return "#D6D6D6"; else return colors[hue % 8]
}

/**
 * @param {Number} hue    Integer number (or -1 for "colorless")
 * @returns {String}      Color code, e.g. "#D1C4E9"
 */
function lightColor(hue) {
    // deep purple, green, amber, blue,
    // red, brown, teal, limehau
    var colors = [
        "#EDE7F6", "#E8F5E9", "#FFF8E1", "#E3F2FD",
        "#FFEBEE", "#EFEBE9", "#E0F2F1", "#F9FBE7"];
    if (hue == -1) return "#F0F0F0"; else return colors[hue % 8]
}
