# Motivation for this module

The situation for handling client-side CSRF token injection was unsatisfying, to
say the least. Without performing significant surgery on the types and method
the Guide uses to generate JavaScript functions, our best option is to modify
the jQuery `$.ajax()` or `$.post()` functions.

There are a grand total of four packages on [npmjs.com](https://npmjs.com) that
show up for "jquery csrf". The most promising is `jquery-csrf-token`. It has two
problems, one technical and one contextual.

1. It does not filter based on the URL, it is a shotgun. Not knowing a lot about
   how Spock generates and validates CSRF tokens or how that could change, we
   should defensively program around the worst case: CSRF tokens are valid for a
   really long time beyond a user's session, and leaking one could be bad.

2. It gets ~40 downloads a month. Let's not let ourselves be `left-pad`ed.

So we will include the source (it's relatively short) and add the modifications
we need, and _also_ provide a nice path forward for building a
single-source-of-truth for client JavaScript for the project. Since
`jquery-csrf-token` uses [Rollup](http://rollupjs.org/), we will too.

We will also use URL parsing to make sure that we only send the CSRF token to
the a relative URI. Rollup will come in handy here because IE11 (ugh) and Opera
Mini (what?) do not support the URL API and so we'll polyfill it.

Other features may be added as needed and will be documented here.
