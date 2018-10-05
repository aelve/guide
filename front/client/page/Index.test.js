import { Selector } from 'testcafe';
import VueSelector from 'testcafe-vue-selectors';

fixture `Index`
  .page `http://localhost:5000`;

test('Navigate to category page', async navigateCategory => {
  await navigateCategory
    .click('.test-btn')
    .navigateTo('http://localhost:5000/haskell')
});

test('Test search input', async inputSearch => {
  // !!! Testcafe-vue-selectors currently dont support vue cumponents loaded by vue-loader
  // const searchInput = VueSelector('v-text-field');
  // await inputSearch
  //   .typeText(searchInput, 'Haskell')
  // .pressKey('enter')
  //   .expect(searchInput.value).eql('Haskell')

  const searchInput = Selector('input[aria-label="Search"]');

  await inputSearch
    .typeText(searchInput, 'Haskell')
    .expect(searchInput.value).eql('Haskell')
    .pressKey('enter')
    .navigateTo('http://aelve.com:4801/haskell/?q=Haskell')
})

// test('Open add category popup', async t => {
//   const addCategoryBtn = VueSelector('add-category-dialog v-btn');
//   const addCategoryDialog = VueSelector('add-category-dialog');
//   const addCatDialogVue = addCategoryDialog.getVue();

//   await t.click(addCategoryBtn).expect(addCatDialogVue.state.isDialogOpen).eql(true);
// });

test('Add category', async addCategory => {
  // const catBtn = VueSelector('b');
  // const rootVue = VueSelector();
  // console.log(catBtn);
  // const catInput = Selector('input[aria-label="Category name"]');
  let num = Math.floor(Math.random() * 10000);
  let catName = 'mytest-' + num;
  
  await addCategory
    // .click(catBtn)
    // button[*data-v-4e53f99f]
    .click('.category-group button')
    .typeText('input[aria-label="Category name"]', 'mytest-' + catName)
    // .click('.add-cat-submit')
    // .expect(Selector('.category group > a > .body-1').innerText).contains(catName)
    // 'h6[data-v-4e53f99f]'
  await addCategory
    .click('.add-cat-submit')
  //   .expect(Selector('.category group > a > .body-1').innerText).contains(catName);
})