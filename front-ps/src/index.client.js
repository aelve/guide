// polyfills
require('es6-promise').polyfill();
// shared js
import './common.js';
// client app
import Client from './Client.purs';

// window.__puxInitialState is the JSON serialized state injected after
// rendering on the server (src/Server.purs).
const initialState = Client.readState(window.__puxInitialState);
// If hot-reloading, hook into each state change and re-render using the last
// state.
if (module.hot) {
  let app = Client.main(window.location.pathname)(window.__puxLastState || initialState)()
  // Hook for pux devtools
  window.__puxApp = app;

  app.state.subscribe((state) => {
    window.__puxLastState = state;
  });

  module.hot.accept();
} else {
  Client.main(window.location.pathname)(initialState)()
}
