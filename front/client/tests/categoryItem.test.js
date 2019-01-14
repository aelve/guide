import { Selector } from 'testcafe'

fixture`ItemAddDelete`
  .page`http://localhost:5000/haskell/data-structures-fum5aqch`

const newItemName = 'mytest-' + new Date().toISOString()

test('Add New Item to category', async t => {
  const addItemBtn = Selector('.add-item-btn')
  const addItemInput = Selector('input[aria-label="Item name"]')

  await t
    .click(addItemBtn)
    .typeText(addItemInput, newItemName)
    .pressKey('enter')

  const articleHeadings = Selector('.article-hd-textlg')
  const articleHeadingsCount = await articleHeadings.count

  // for (let i = 0; i < articleHeadingsCount; i++) {
  //   await t.expect(Selector('.article-hd-textlg').nth(i).innerText).contains(newItemName)
  // }
  await t.expect(Selector('.article-hd-textlg').nth(articleHeadingsCount - 1).innerText).contains(newItemName)
})

test('Delete Item from category', async t => {
  const delItemBtn = Selector(() => {
    const buttons = document.querySelectorAll('.item-del-btn')
    let last = buttons[buttons.length - 1]
    return last
  })

  const delSubmitBtn = Selector('.confirm-btn')
  await t
    .click(delItemBtn)
    .click(delSubmitBtn)

  const articleHeadings = Selector('.article-hd-textlg')
  const articleHeadingsCount = await articleHeadings.count

  for (let i = 0; i < articleHeadingsCount; i++) {
    await t.expect(articleHeadings.nth(i).innerText).notContains(newItemName)
  }
})
