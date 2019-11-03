// Set property on state
// setUser: set('user')
// commit('setUser', { name: 'foo' })
export const set = key => (state, val) => { state[key] = val }

// https://gist.github.com/rhythnic/6521495650a215ccab8bf7120949fb7d more helpers