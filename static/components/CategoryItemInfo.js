////////////////////////////////////////////////////////////////////////////
// Information about the item, along with some controls
//
// Events:
//   * move-item-up      User wants to move the item up
//   * mode-item-down    User wants to move the item down
//   * delete-item       User wants to delete the item
//   * edit-item-info    User wants to edit item info
//
////////////////////////////////////////////////////////////////////////////

Vue.component('CategoryItemInfo', {
    props: {
        // Contents of the item
        item: {type: Object, required: true},
    },

    computed: {
        // Item group, e.g. "PCRE-based" or "POSIX-based" for regex libraries
        group: function() {
            return this.item.group_ || "other" },
        // Link to the "official site" or something
        link: function () {
            return this.item.link || null },
        // Link to the Hackage page (for Haskell packages)
        hackageLink: function () {
            return (this.item.kind.tag == "Library") ? this.item.kind.contents :
                   (this.item.kind.tag == "Tool") ? this.item.kind.contents :
                   null },
    },

    // TODO: this template is somewhat messy; styles should be moved out;
    // spans and divs used like here are weird, too
    template: `
<div>
    <div style="font-size:23px; line-height:27px;">
        <a class="anchor" :href="item.link_to_item">#</a>
    </div>

    <div style="font-size:23px; line-height:27px;">
        <a v-if="link !== null" :href="link" class="item-name">{{ item.name }}</a>
        <span v-else>{{ item.name }}</span>
        <template v-if="hackageLink !== null">
            (<a :href="hackageLink">Hackage</a>)
        </template>
    </div>

    <div class="item-group" style="line-height:27px;">
        {{ group }}
    </div>

    <div class="controls">
        <span>
            <a href="#" class="move-item-up"
                @click.prevent="this.$emit('move-item-up')">
                <img src="/arrow-thick-top.svg" alt="up" title="move item up"></a>
            <a href="#" class="move-item-down"
                @click.prevent="this.$emit('move-item-down')">
                <img src="/arrow-thick-bottom.svg" alt="down" title="move item down"></a>
        </span>
        <span>
            <a href="#" class="edit-item-info"
                @click.prevent="this.$emit('edit-item-info')">
                <img src="/cog.svg" alt="edit" title="edit item info"></a>
            <span style="margin-left: 0.4em"></span>
            <a href="#" class="delete-item"
                @click.prevent="this.$emit('delete-item')">
                <img src="/x.svg" alt="delete" title="delete item"></a>
        </span>
    </div>
`,
});

////////////////////////////////////////////////////////////////////////////
// Item info edit form
//
// Shown when you press the cog in the item titlebar.
////////////////////////////////////////////////////////////////////////////

Vue.component('CategoryItemInfoEdit', {
    props: {
        // Contents of the item
        item: {type: Object, required: true},
    },

    // We use 'autocomplete=off' everywhere due to this:
    // http://stackoverflow.com/q/8311455
    template: `
<form class="item-info-edit-form" onsubmit="submitItemInfo('{{item.uid}}', this); return false;">
  <label for="name">Name</label>
  <input id="name" name="name" value="{{item.name}}"
          type="text" autocomplete="off">

  <label for="kind">Kind</label>
  <select id="kind" name="kind" autocomplete="off">
    {{! possible_kinds would have stuff like “library”, “tool”, “other” }}
    {{#possible_kinds}}
      <option value="{{name}}" {{%selectIf selected}}>{{caption}}</option>
    {{/possible_kinds}}
  </select>
 
  <label for="hackage-name">Name on Hackage</label>
  <input id="hackage-name" name="hackage-name" value="{{#item.kind.hackageName}}{{.}}{{/item.kind.hackageName}}"
          type="text" autocomplete="off">

  <label for="site">Site (optional)</label>
  <input id="site" name="link" value="{{item.link}}"
          type="text" autocomplete="off">
  
<div class="form-group">
  <label for="group">Group</label>
  {{! When “new group” is selected in the list, we show a field for
      entering new group's name }}
  <select id="group" name="group" onchange="itemGroupSelectHandler(this);"
          autocomplete="off">
    <option value="-" {{%selectIf item_no_group}}>-</option>
    {{# category_groups }}
      <option value="{{name}}" {{%selectIf selected}}>{{name}}</option>
    {{/ category_groups }}
    <option value="">New group...</option>
  </select>
  
  <input hidden class="custom-group-input" name="custom-group"
         type="text" autocomplete="off">
  </div>

  <div class="form-btn-group">
    <input value="Save" class="save" type="submit">
    <input value="Cancel" class="cancel" type="button"
          onclick="itemInfoCancelEdit('{{item.uid}}');">
  </div>
</form>
`,
});
