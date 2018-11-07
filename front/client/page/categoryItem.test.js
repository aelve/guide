import { Selector } from 'testcafe'
import VueSelector from 'testcafe-vue-selectors'

fixture `ItemAddDelete`
  .page `http://localhost:5000/haskell`;

test('Add New Item to category', async addItem => {
  const addItemBtn
  const addItemInput = Selector('input[aria-label="Item name"]')
  const itemSubmitBtn
  
  await addItem
  // необходимо кликнуть первую кнопку добавления item -> ввести название item -> кликнуть на submit
  // Проверить появился ли item с таким name
})

test('Delete Item from category', async deleteItem => {
  const delItemBtn
  const delSubmitBtn
  
  await deleteItem
})