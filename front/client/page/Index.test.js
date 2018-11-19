import { Selector, ClientFunction } from 'testcafe'
import VueSelector from 'testcafe-vue-selectors'

const baseUrl = `http://localhost:5000`
const getLocation = ClientFunction(() => document.location.href)
// !!! Testcafe-vue-selectors currently dont support vue cumponents loaded by vue-loader

fixture`Index`
  .page(baseUrl)

test('Navigate to category page', async t => {
  await t
    .click('.test-btn')
    .expect(getLocation()).contains(`${baseUrl}/haskell`)
})

test('Test search input', async inputSearch => {

  const searchInput = Selector('input[aria-label="Search"]')

  await inputSearch
    .typeText(searchInput, 'Haskell')
    .expect(searchInput.value).eql('Haskell')
    .pressKey('enter')
    .expect(getLocation()).eql(`${baseUrl}/haskell/search/results?query=Haskell`)
})

test('Add category', async t => {
  const categoryGroups = Selector('.category-group')
  const groupsCount = await categoryGroups.count

  if (!categoryGroups || !groupsCount) {
    return
  }

  for (let i = 0; i < groupsCount; i++) {
    const currentGroup = categoryGroups.nth(i)
    const categoryGroupName = currentGroup.child('.category-group-name').innerText

    const addButton = currentGroup.child('.add-category-btn')

    const newCategoryName = 'mytest-' + new Date().toISOString()
    await t.click(addButton)
    await t.expect(Selector('.aria-label="Group"').innerText).eql(categoryGroupName)
    await t
      .typeText('input[aria-label="Category name"]', newCategoryName)
      .click('.add-category-submit-btn')
    // Cause after adding category it opens in new tab we need to navigate back, because TestCafe doest support opening new tabs
    // https://github.com/DevExpress/testcafe/issues/2293
    await t.navigateTo(`${baseUrl}`)
    await t.expect(currentGroup.find(node => node.innerText === newCategoryName).exists)
  }
})