import 'reflect-metadata'
import _get from 'lodash/get'
import { createApp } from './app'

export default async context => {
  return new Promise((resolve, reject) => {
    const { app, router, store } = createApp()

    router.push(context.url)

    router.onReady(async () => {
      const matchedComponents = router.getMatchedComponents()

      if (!matchedComponents.length) {
        return reject({
          code: 404,
          error: new Error('no component matched')
        })
      }
      try {
        const matchedComponentsAndChildren = matchedComponents
          .reduce((acc, matchedComponent) => {
            const componentAndItsChildren = getComponentAndItsChildren(matchedComponent)
            acc = acc.concat(componentAndItsChildren)
            return acc
          }, [])

        await Promise.all(matchedComponentsAndChildren.map((Component) => {
          const asyncDataFunc = _get(Component, 'options.methods.asyncData')
          if (typeof asyncDataFunc === 'function') {
            return asyncDataFunc({
              store,
              route: router.currentRoute
            })
          }
        }))
        context.state = store.state
        resolve(app)
      } catch (e) {
        reject
      }
    }, reject)
  })
}

function getComponentAndItsChildren(component, result?) {
  if (!result) {
    result = []
  }
  if (!result.includes(component)) {
    result.push(component)
  }
  const children = Object.values(component.options.components)
    // Parent component is also presents in components object
    .filter(x => x !== component)
  children.forEach(x => getComponentAndItsChildren(x, result))

  return result
}
