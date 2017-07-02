
require('es6-promise').polyfill();
require('isomorphic-fetch');

exports.fetchImpl = function(uri, done, fail) {
  return fetch(uri)
  	.then( function(response) {
      const status = response.status;
  		if (status >= 400) {
        return fail('Bad response from server -> status : ' + status);
  		}
  		return response.json();
  	})
    .then(function(data) {
  		done(data)();
  	});
  };
