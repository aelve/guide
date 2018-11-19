import { Selector } from 'testcafe'
import VueSelector from 'testcafe-vue-selectors'

fixture `ItemAddDelete`
  .page `http://localhost:5000/haskell`;

test('Add New Item to category', async addItem => {
  const addItemBtn = Selector('.add-item-btn')
  const addItemInput = Selector('input[aria-label="Item name"]')
  const itemSubmitBtn = Selector('.add-cat-submit')
  
  await addItem
    .click(addItemBtn)
    .typeText(addItemInput, 'testcafe-item')
    .pressKey('enter')
  for (let i = 0; Selector('.article-hd-textlg').length; i++) {
    await addItem.expect(Selector('.article-hd-textlg').nth(i).innerText).contains('testcafe-item')
  }
})

test('Delete Item from category', async deleteItem => {
  const delItemBtn = Selector(() => {
    const buttons = document.querySelectorAll('.item-del-btn')
    let last = buttons[buttons.length - 1]
    return last
  })

  const delSubmitBtn = Selector('.confirm-btn')
  
  await deleteItem
    .click(delItemBtn)
    .click(delSubmitBtn)
  for (let i = 0; Selector('.article-hd-textlg').length; i++) {
    await deleteItem.expect(Selector('.article-hd-textlg').nth(i).innerText).notContains('testcafe-item')
  }  
})