import jquery from 'jquery';
// export jQuery to the globals.
window.$ = window.jquery = window.jQuery = jquery;

import csrfProtection from './csrfProtection.js';

export default { 
  csrfProtection
};
