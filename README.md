# intlc

**Disclaimer**: Currently for internal use but feedback/external usage welcome.

Compile ICU messages into code.

```
Usage: intlc COMMAND
  Compile ICU messages into code.

Available options:
  -h,--help                Show this help text

Available commands:
  compile
  flatten
```

For example:

```
$ intlc compile ./translations.json -l en-US
```

## Contributing

Check out `ARCHITECTURE.md`. Currently building against GHC 8.10.7.

## Publishing new release

Pushing a git tag will trigger a build on github actions. It will build binaries for different OS and create a new release with them attached.
By convention tags will have the following format `v{major}.{minor}.{patch}`

We don't fully have everything automated so here's what you should do whenever you need to publish a new intlc version:

- Bump `intlc.cabal`
- `git tag v{version} && git push {remote} v{version}`
- Bump intlc version in `@unsplash/intlc` postinstall script to match the latest intlc https://github.com/unsplash/intlc/blob/master/npm/package.json#L7
- Bump `@unsplash/intlc` package.json version and publish **only** when the brand new release is available on github.
