// shared js
import '../../shared.js';
// client app
var Client = require('./CategoryOverview.purs');
const app = Client.main(window.location.pathname)(window.__puxLastState || Client.initialState)()

app.state.subscribe(function (state) {
 window.__puxLastState = state;
});
