import URL from './url-polyfill';
import { csrfPrefilter as oldPrefilter, enable as jqueryCsrfEnable } from './jquery-csrf-token';

// Now we patch in our own prefilter from url-parse, and layer it with the one from jqueryCsrfToken.
function originFilter(options, ...args) {
    let docOrigin = document.location.origin;
    let reqOrigin = (new URL(options.url, document.location)).origin;

    // For now, only test to make sure the origins are the same.
    // TODO: Filter to say, a /api/ prefix?
    if (docOrigin === reqOrigin) {
      oldPrefilter(options, ...args);
    }
}

function enable(csrfKey, csrfValue) {
  jqueryCsrfEnable(csrfValue, {
    key: csrfKey,
    prefilter: originFilter,
    retry: null,
  });
};

export default {
  enable
};
