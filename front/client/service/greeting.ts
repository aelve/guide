import { Injectable } from '@vert/core'

@Injectable()
class GreetingService {
  greet (name: string) {
    console.log(`Hello, ${name}!`)
  }
}

export {
  GreetingService
}
