import uniqid from 'uniqid'
import axios from 'axios'
import yargs from 'yargs'
import categoryPathToId from '../client/helpers/categoryPathToId'
import config from './config.tests'
import { sel, testIdAttribute, getLocation, goBack } from './utils'
import { CategoryItemModel } from './models'

const baseUrl = config.baseUrl

async function createCategory (t, { categoryName, groupName } = {}) {
  const createCategoryBtn = sel('CreateCategoryBtn').nth(0)
  const nameInput = sel('AddCategoryDialog-NameInput')
  const groupInput = sel('AddCategoryDialog-GroupInput')
  const submitBtn = sel('AddCategoryDialog-SubmitBtn')

  categoryName = categoryName || `testCategory-${uniqid()}`
  groupName = groupName || `testGroup-${uniqid()}`

  await t
    .click(createCategoryBtn)
    .typeText(nameInput, categoryName, { replace: true })
    .typeText(groupInput, groupName, { replace: true })
    .click(submitBtn)

  // wait for redirection to category page happens
  const redirectionWaitTime = 2500
  await t.wait(redirectionWaitTime)
  const categoryParams = { categoryName, groupName }
  const duplicateCategoryDialog = sel('DuplicateCategoryDialog', { timeout: 2000 })
  const isDuplicateCategory = await duplicateCategoryDialog() && await duplicateCategoryDialog.exists && await duplicateCategoryDialog.visible
  if (isDuplicateCategory) {
    await t.click(sel('DuplicateCategoryDialog-ConfirmBtn'))
    await t.wait(redirectionWaitTime)
  }

  return categoryParams
}

async function clickBtnIfVisible (t, btnSelector) {
  const btn = sel(btnSelector)
  const isVisible = await btn.visible

  if (isVisible) {
    await t.click(btn)
  }
}

async function openCategoryMobileActionsMenu (t, index) {
  await clickBtnIfVisible(t, 'CategoryHeader-MobileMenuBtn', index)
}

async function createItem (t, { name, hackage, link } = {}) {
  name = name || `testItem-${uniqid()}`
  hackage = hackage === false
    ? hackage
    : hackage || `${uniqid()}`
  link = link === false
    ? link
    : link || `https://aelve.com/${uniqid()}`

  const typeOptions = { replace: true }

  await openCategoryMobileActionsMenu(t)

  await t
    .click(sel('CategoryHeader-NewItemBtn'))
    .typeText(sel('AddItemDialog-NameInput'), name, typeOptions)
  if (hackage) {
    await t.typeText(sel('AddItemDialog-HackageInput'), hackage, typeOptions)
  }
  if (link) {
    await t.typeText(sel('AddItemDialog-LinkInput'), link, typeOptions)
  }
  await t.click(sel('AddItemDialog-SubmitBtn'))

  return new CategoryItemModel(t, { name, hackage, link })
}

const testFunctions = {
  async resizeWindowIfMobile (t) {
    const isMobile = yargs.argv.mobile

    if (isMobile) {
      const mobileWidth = 320
      const mobileHeight = 568
      await t.resizeWindow(mobileWidth, mobileHeight)
    }
  },

  async createCategoryAndDuplicate (t) {
    const { groupName, categoryName } = await createCategory(t)
    // after creating category it automatically navigates to category page
    await goBack()
    await createCategory(t, { categoryName, groupName })
    await goBack()

    const groupTitle = sel('Categories-CategoryGroup-Title').withText(groupName)
    const group = groupTitle.parent(testIdAttribute('Categories-CategoryGroup'))

    const numberOfCategories = group
      .find(testIdAttribute('Categories-CategoryTitle'))
      .withText(categoryName)
      .count
    await t
      .expect(numberOfCategories)
      .eql(2)
  },

  async editCategory (t) {
    const { categoryName } = await createCategory(t)
    const item = await createItem(t)

    const traitsSection = sel('CategoryItem-TraitsSection')
    const ecosystemSection = sel('CategoryItem-EcosystemSection')
    const notesSection = sel('CategoryItem-NotesSection')

    await t
      .expect(item.nameTitle.exists).ok()
      .expect(traitsSection.exists).ok()
      .expect(ecosystemSection.exists).ok()
      .expect(notesSection.exists).ok()

    await openCategoryMobileActionsMenu(t)
    await t.click(sel('CategoryHeader-CategorySettingsBtn'))

    const newTitle = categoryName + 'a'
    const newGroup = `group-${uniqid()}`
    await t
      .typeText(sel('CategorySettings-TitleInput'), newTitle, { replace: true })
      .typeText(sel('CategorySettings-GroupInput'), newGroup, { replace: true })
      .click(sel('CategorySettings-ItemTraitsSectionCheckbox'))
      .click(sel('CategorySettings-ItemEcosystemSectionCheckbox'))
      .click(sel('CategorySettings-ItemNotesSectionCheckbox'))
      .click(sel('CategorySettings-SubmitBtn'))
      .expect(sel('CategoryHeader-Title').innerText).eql(newTitle)
      .expect(sel('CategoryHeader-Group').innerText).eql(newGroup)
      .expect(traitsSection.exists).notOk()
      .expect(ecosystemSection.exists).notOk()
      .expect(notesSection.exists).notOk()
    // TODO check status changing
  },

  async deleteCategory (t) {
    const { categoryName } = await createCategory(t)
    await goBack()

    const createdCategoryTitle = sel('Categories-CategoryTitle').withText(categoryName)
    const categoryDeleteBtn = sel('CategoryHeader-CategoryDeleteBtn')
    const submitCategoryDeleteBtn = sel('DeleteCategoryDialog-ConfirmBtn')
    await t
      .expect(createdCategoryTitle.exists).ok()
      .click(createdCategoryTitle)
    await openCategoryMobileActionsMenu(t)
    await t
      .click(categoryDeleteBtn)
      .click(submitCategoryDeleteBtn)
      .navigateTo(baseUrl)
      .expect(createdCategoryTitle.exists).notOk()
  },

  async createItemWithOptionalParams (t) {
    await createCategory(t)
    const item = await createItem(t)

    await t
      .expect(item.nameTitle.exists).ok()
      .expect(item.linkHref).eql(item.linkValue)
      .expect(item.hackageLinkHref).contains(item.hackage)
  },

  async createItemNoOptionalParams (t) {
    await createCategory(t)
    const item = await createItem(t, { hackage: false, link: false })

    await t
      .expect(item.nameTitle.exists).ok()
      .expect(item.linkHref).eql(undefined)
      .expect(item.hackageLink.exists).notOk()
  },

  async deleteItem (t) {
    await createCategory(t)
    const item = await createItem(t)

    await t.expect(item.nameTitle.exists).ok()
    await item.delete()
    await t.expect(item.nameTitle.exists).notOk()
  },

  async editItem (t) {
    await createCategory(t)
    const item = await createItem(t)

    const editedItem = {
      name: `testItem-${uniqid()}`,
      hackage: `testItemHackage-${uniqid()}`,
      link: `https://aelve.com/${uniqid()}`
    }

    await t.expect(item.nameTitle.exists).ok()
    await item.toggleInfoEdit()
    await t
      .typeText(sel('CategoryItemToolbar-InfoEdit-NameInput'), editedItem.name, { replace: true })
      .typeText(sel('CategoryItemToolbar-InfoEdit-HackageInput'), editedItem.hackage, { replace: true })
      .typeText(sel('CategoryItemToolbar-InfoEdit-LinkInput'), editedItem.link, { replace: true })
      .click(sel('CategoryItemToolbar-InfoEdit-SaveBtn'))
    item.name = editedItem.name
    item.hackage = editedItem.hackage
    item.linkValue = editedItem.linkValue
    await t
      .expect(item.nameTitle.exists).ok()
      .expect(item.hackageLinkHref).contains(editedItem.hackage)
      .expect(item.linkHref).eql(editedItem.link)
  },

  async moveItems (t) {
    await createCategory(t)
    const firstItem = await createItem(t)
    const secondItem = await createItem(t)

    const itemTitle = sel('CategoryItemToolbar-ItemName')
    const firstFoundItemName = itemTitle.nth(0).innerText
    const secondFoundItemName = itemTitle.nth(1).innerText

    await t
      .expect(firstFoundItemName).eql(firstItem.name)
      .expect(secondFoundItemName).eql(secondItem.name)

    await firstItem.moveDown()
    await t
      .expect(firstFoundItemName).eql(secondItem.name)
      .expect(secondFoundItemName).eql(firstItem.name)
    await firstItem.moveUp()
    await t
      .expect(firstFoundItemName).eql(firstItem.name)
      .expect(secondFoundItemName).eql(secondItem.name)
  },

  async mergeConflicts (t) {
    await createCategory(t)

    const categoryPath = (await getLocation()).split('/').pop()
    const categoryId = categoryPathToId(categoryPath)

    const editDescriptionBtn = sel('Category-EditDescriptionBtn')
    const descriptionEditor = sel('CategoryDescription-Editor').find('.CodeMirror textarea')
    const descriptionEditorSaveBtn = sel('CategoryDescription-Editor').find(testIdAttribute('MarkdownEditor-SaveBtn'))
    const descriptionText = sel('CategoryDescription-Content').innerText
    const conflictDialog = sel('ConflictDialog')

    // because testCafe doesn't support multiple tabs/windows, to emulate merged conflict api requests executed manually
    // https://github.com/DevExpress/testcafe/issues/912
    const updateDescription = ({ original, modified }) => axios
      .put(`${baseUrl}/api/category/${categoryId}/notes`, {
        original,
        modified
      })

    const description = `testDescription-${uniqid()}`
    await t
      .click(editDescriptionBtn)
      .typeText(descriptionEditor, description)
      .click(descriptionEditorSaveBtn)
      .expect(descriptionText).eql(description)

    const newDescription = `testDescription-${uniqid()}`
    // update description manually so in browser it's still equal to "description" variable
    await updateDescription({ original: description, modified: newDescription })

    // when we save current description even without modifying conflict dialog should popup,
    // cause original version on server is different
    await t
      .click(editDescriptionBtn)
      .click(descriptionEditorSaveBtn)
      .click(sel('ConflictDialog-SubmitLocalBtn'))
      .expect(descriptionText).eql(description)

    await updateDescription({ original: description, modified: newDescription })

    await t
      .click(editDescriptionBtn)
      .click(descriptionEditorSaveBtn)
      .click(sel('ConflictDialog-SubmitServerBtn'))
      .expect(descriptionText).eql(newDescription)

    await updateDescription({ original: newDescription, modified: description })

    await t
      .click(editDescriptionBtn)
      .click(descriptionEditorSaveBtn)

    const mergedText = await conflictDialog
      .find(testIdAttribute('MarkdownEditor-OriginalTextarea'))
      .value

    await t.click(sel('ConflictDialog-SubmitMergedBtn'))
    await t.expect(descriptionText).eql(mergedText)
  }
}
export default testFunctions
