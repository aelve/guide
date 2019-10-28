import { sel, testIdAttribute } from './utils'

class CategoryItemModel {
  constructor (t, { name, hackage, link }) {
    this.t = t
    this.name = name
    this.hackage = hackage
    this.linkValue = link
  }
  get nameTitle () {
    return sel('CategoryItemToolbar-ItemName').withText(this.name)
  }
  get toolbar () {
    return this.nameTitle.parent(testIdAttribute('CategoryItemToolbar'))
  }
  get linkHref () {
    return this.nameTitle.getAttribute('href')
  }
  get hackageLink () {
    return this.toolbar.find(testIdAttribute('CategoryItemToolbar-HackageLink'))
  }
  get hackageLinkHref () {
    return this.hackageLink.getAttribute('href')
  }
  get mobileMenuBtn () {
    return this.toolbar.find(testIdAttribute('CategoryItemToolbar-MobileMenuBtn'))
  }
  get moveDownBtn () {
    return this.toolbar.find(testIdAttribute('CategoryItemToolbar-MoveDownBtn'))
  }
  get moveUpBtn () {
    return this.toolbar.find(testIdAttribute('CategoryItemToolbar-MoveUpBtn'))
  }
  get editInfoBtn () {
    return this.toolbar.find(testIdAttribute('CategoryItemToolbar-EditInfoBtn'))
  }
  get deleteBtn () {
    return this.toolbar.find(testIdAttribute('CategoryItemToolbar-DeleteBtn'))
  }

  async openMobileActionsMenu () {
    const isMobile = await this.mobileMenuBtn.visible
    if (isMobile) {
      await this.t.click(this.mobileMenuBtn)
    }
  }

  async clickActionBtn (btn) {
    await this.openMobileActionsMenu()
    await this.t.click(btn.filterVisible())
  }

  async moveUp () {
    await this.clickActionBtn(this.moveUpBtn)
  }

  async moveDown () {
    await this.clickActionBtn(this.moveDownBtn)
  }

  async delete () {
    await this.clickActionBtn(this.deleteBtn)
    await this.t.click(sel('ItemDeleteDialog-ConfirmBtn'))
  }

  async toggleInfoEdit () {
    await this.clickActionBtn(this.editInfoBtn)
  }
}

export {
  CategoryItemModel
}
