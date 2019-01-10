interface IConfirmDialogProps {
  text: string,
  confirmBtnText?: string,
  cancelBtnText?: string
}
/**
 * Use only for vue components methods.
 * Executes '_confirm' function (see confirmDialogMixin.ts) with provided options before executing following method.
 * If not confirmed method is not executed.
 * @param {Object} options - options passed to _confirm function
 * @param {String} options.text
 * @param {String} options.confirmBtnText
 * @param {String} options.cancelBtnText
 */
export default function (options: IConfirmDialogProps) {
  return function (target: any, propertyKey: string, descriptor: PropertyDescriptor) {
    const originalFunction = descriptor.value
    descriptor.value = async function (...args) {
      const isConfirmed = await this._confirm(options)
      if (!isConfirmed) {
        return
      }

      originalFunction.apply(this, args)
    }
  }
}
