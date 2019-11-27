export default function (event) {
  const { target } = event
  const dialogSelector = '.v-dialog__content'
  const isDialog = target.classList.contains(dialogSelector) || target.closest(dialogSelector)
  const nodeName = target.nodeName.toLowerCase()
  const typingElements = ['input', 'textarea']
  const isTypingElement = typingElements.includes(nodeName)

  return isDialog || isTypingElement
}
