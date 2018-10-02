import { Selector } from 'testcafe';
// import VueSelector from 'testcafe-vue-selectors';

fixture `Index`
  .page `http://localhost:5000`;

test('Navigate to category page', async navigateCategory => {
  await navigateCategory
    .click('.test-btn')
    .navigateTo('http://localhost:5000/haskell')
});

test('Test search input', async inputSearch => {
  const searchInput = Selector('input[aria-label="Search"]');

  await inputSearch
    .typeText(searchInput, 'Haskell')
    .expect(searchInput.value).eql('Haskell')
})

// test('Open add category popup', async t => {
//   const addCategoryBtn = VueSelector('add-category-dialog v-btn');
//   const addCategoryDialog = VueSelector('add-category-dialog');
//   const addCatDialogVue = addCategoryDialog.getVue();

//   await t.click(addCategoryBtn).expect(addCatDialogVue.state.isDialogOpen).eql(true);
// });