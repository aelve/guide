
require('isomorphic-fetch');

exports.fetchImpl = function(uri, successCb, errCb) {
  return function () {
    return fetch(uri)
    	.then( function(response) {
        const status = response.status;
    		if (status >= 400) {
          return errCb('Bad response from server -> status : ' + status)();
    		}
    		return response.json();
    	})
      .then(function(data) {
        successCb(data)();
      });
    };
  };
