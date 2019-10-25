import { Selector, ClientFunction } from 'testcafe'

const getLocation = ClientFunction(() => document.location.href)
const goBack = ClientFunction(() => window.history.back())
const testIdAttribute = id => `[data-testid="${id}"]`
const sel = (id, options) => Selector(testIdAttribute(id), options)

export {
  getLocation,
  goBack,
  testIdAttribute,
  sel
}
