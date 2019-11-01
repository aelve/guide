import testFunctions from './testFunctions'
import config from './config.tests'

fixture`Index`
  .page(config.baseUrl)
  .beforeEach(async t => {
    // sometimes it takes long for index page to load and tests can fail
    await t.wait(2000)
  })
  .afterEach(async t => {
    const { error } = await t.getBrowserConsoleMessages();
    const failMessage = `\n\nFollowing errors occuried in console during the test:\n${error.map(x => `${x}\n`)}\n\n`
    await t.expect(error.length).notOk(failMessage)
  })
// TODO add hook that checks for console errors on each test and fail if errors exist

test('Resize window if mobile test', testFunctions.resizeWindowIfMobile)
test('Create category, create category with duplicate name', testFunctions.createCategoryAndDuplicate)
test('Edit category', testFunctions.editCategory)
test('Delete category', testFunctions.deleteCategory)
test('Create item with optional parameters', testFunctions.createItemWithOptionalParams)
test('Create item without optional parameters', testFunctions.createItemNoOptionalParams)
test('Delete item', testFunctions.deleteItem)
test('Edit item', testFunctions.editItem)
test('Move items', testFunctions.moveItems)
test('Merge conflicts', testFunctions.mergeConflicts)
