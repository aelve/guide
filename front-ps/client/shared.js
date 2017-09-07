// common shared js
import '../common/shared.js';
// polyfills
require('es6-promise').polyfill();

export const initClient = function (client) {
  // window.__puxInitialState is the JSON serialized state injected after
  // rendering on the server (src/Server.purs).
  const initialState = client.readState(window.__puxInitialState);

  const app = client.main(window.location.pathname)(window.__puxLastState || initialState)();

  app.state.subscribe(function (state) {
   window.__puxLastState = state;
  });

}
