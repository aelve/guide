import { Selector } from 'testcafe';

fixture `Index`
  .page `http://localhost:5000`;

test('Navigate to category page', async navigateCategory => {
  await navigateCategory
    .click('.test-btn')
    .navigateTo('http://localhost:5000/haskell')
});

