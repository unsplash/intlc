# @unsplash/intlc

_This package is public for convenience but has peer requirements over `fp-ts` and all so it wouldn't necessarily make sense for all kinds of codebases_

Wrapper that installs the requested `intlc` binary for your OS and make it available as local dependency.

### Publishing

This package contains a `postinstall` script that downloads an intlc binary targeted by a specific version. That release must be available on github, otherwise the script will fail.
As convention this package version follows `intlc` version. Anything after the dash is specific to this package, for instance if you need to fix a bug.

e.g

```
intlc@0.2.0 -> @unsplash/intlc@0.2.0-0
intlc@0.2.1 -> @unsplash/intlc@0.2.1-0
intlc@0.2.1 -> @unsplash/intlc@0.2.1-0{,1,2,3,4,5,6,...} // This last number can be bumped independently of intlc's binary
```

We configure this package as a `bin` in `package.json`. Note that we must create a `intlc` placeholder file in the dist folder because NPM checks for that file prior to run the `postinstall` script.
We've inspired ourselves to what purescript does: https://github.com/purescript/purescript/commit/32b0c1fb451a58c5e1f3d8ed3acbfb905b1ff155