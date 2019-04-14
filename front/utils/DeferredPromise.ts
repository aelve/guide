// export default class DeferredPromise<T = any> extends Promise<T> {
//   readonly [Symbol.toStringTag]: 'Promise'
//   // public resolve: (value?: T | PromiseLike<T>) => void
//   // public reject: (reason?: any) => void
//   constructor () {
//     super((resolve, reject) => {
//       this.resolve = resolve
//       this.reject = reject
//     })
//   }
// }

// TODO refactor

export default class DeferredPromise {
  resolve: any
  reject: any
  _promise: any
  catch: any
  then: any
  constructor () {
    this._promise = new Promise((resolve, reject) => {
      // assign the resolve and reject functions to `this`
      // making them usable on the class instance
      this.resolve = resolve
      this.reject = reject
    })
    // bind `then` and `catch` to implement the same interface as Promise
    this.then = this._promise.then.bind(this._promise)
    this.catch = this._promise.catch.bind(this._promise)
    this[Symbol.toStringTag] = 'Promise'
  }
}


// export type PromiseStatus = 'resolved' | 'rejected' | 'pending'

// export default class DeferredPromise<T = any> {

//   static RESOLVED: string = 'resolved'
//   static REJECTED: string = 'rejected'
//   static PENDING: string = 'pending'

//   static resolve<T> (value?: T): DeferredPromise<T> {
//     const promise = new DeferredPromise<T>()
//     promise.resolve(value)
//     return promise
//   }

//   static reject<T> (reason: any): DeferredPromise<T> {
//     const promise = new DeferredPromise<T>()
//     promise.reject(reason)
//     return promise
//   }

//   private _promise: Promise<T>
//   private _resolve: ((value: T) => any)
//   private _reject: ((error: any) => any)
//   private _status: PromiseStatus

//   constructor (callback?: (deferred: DeferredPromise<T>) => any) {
//     this._status = DeferredPromise.PENDING as PromiseStatus

//     this._promise = new Promise<T>((resolve: any, reject: any) => {
//       this._resolve = resolve
//       this._reject = reject

//       if (callback !== void 0) { callback.call(this, this) }
//     }).then((value: T) => {
//       this._status = DeferredPromise.RESOLVED as PromiseStatus
//       return value
//     }, (error: any) => {
//       this._status = DeferredPromise.REJECTED as PromiseStatus
//       throw error
//     })
//   }

//   get status (): PromiseStatus {
//     return this._status
//   }

//   get promise (): Promise<T> {
//     return this._promise
//   }

//   resolve (value?: T): void {
//     if (this._status === DeferredPromise.PENDING) {
//       this._resolve(value)
//     } else {
//       throw new TypeError('promise already resolved/rejected')
//     }
//   }

//   reject (reason: any): void {
//     if (this._status === DeferredPromise.PENDING) {
//       this._reject(reason)
//     } else {
//       throw new TypeError('promise already resolved/rejected')
//     }
//   }

//   then<A = any> (onFulfilled: ((result: T) => A), onRejected?: ((reason: any) => A)): DeferredPromise<A> {
//     return new DeferredPromise<A>((deferred: DeferredPromise<A>) => {
//       this._promise.then((result: T) => {
//         try {
//           deferred.resolve(onFulfilled(result))
//         } catch (error) {
//           deferred.reject(error)
//         }
//       }, (reason: any) => {
//         if (onRejected === void 0) {
//           deferred.reject(reason)
//         } else {
//           try {
//             deferred.resolve(onRejected(reason))
//           } catch (error) {
//             deferred.reject(error)
//           }
//         }
//       })
//     })
//   }

//   catch<A = any> (onRejected: ((reason: any) => A)): DeferredPromise<A | T> {
//     return new DeferredPromise<A | T>((deferred: DeferredPromise<A | T>) => {
//       this._promise.then((result: T) => {
//         deferred.resolve(result)
//       }, (reason: any) => {
//         try {
//           deferred.resolve(onRejected(reason))
//         } catch (error) {
//           deferred.reject(error)
//         }
//       })
//     })
//   }
// }
