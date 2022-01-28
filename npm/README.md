# @unsplash/intlc

_This package is public for convenience but has peer requirements over `fp-ts` and all so it wouldn't necessarily make sense for all kinds of codebases_

Wrapper that installs the requested `intlc` binary for your OS and make it available as local dependency.

### Publishing

We configure this package has a `bin` in `package.json`. Note that we must create a `intlc` placeholder file in the dist folder because NPM checks for that file prior to run the `postinstall` script.
We've inspired ourselves to what purescript does: https://github.com/purescript/purescript/commit/32b0c1fb451a58c5e1f3d8ed3acbfb905b1ff155