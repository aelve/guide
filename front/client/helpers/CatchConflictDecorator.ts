/**
 * Decorator is used for functions that update string values such as category description, category item summary/ecosystem/trait, etc.
 * Functions must have one object argument with at least two properties: 'original' and 'modified'.
 * This properties are used in every API request that can have conflicts.
 * Also, decorator needs 'openConflictDialog' function to be defined in component. For this you can add conflictDialogMixin or define your own function to resolve conflict.
 */
export default function (target: any, propertyKey: string, descriptor: PropertyDescriptor) {
  const originalFunction = descriptor.value

  async function catchConflict (argsObject: object) {
    try {
      await originalFunction.call(this, argsObject)
    } catch (err) {
      if (err.response && err.response.status === 409) {
        const serverModified = err.response.data.server_modified
        const modified = err.response.data.modified
        const merged = err.response.data.merged
        const resolvedConflict = await this.openConflictDialog({ serverModified, modified, merged })

        // We use this function again in case of new conflict occurs after resolving this one
        catchConflict.call(this, {
          ...argsObject,
          original: serverModified,
          modified: resolvedConflict
        })
      } else {
        throw err
      }
    }
  }

  descriptor.value = catchConflict
}
