# intlc

**Disclaimer**: Currently for internal use but feedback/external usage welcome.

## Usage

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

### Backends

At present, the following backends (compilation targets) are supported:

- TypeScript (`ts`, default)
- TypeScript/React (`tsx`)

### Schema

Translation files should be encoded as JSON and might look something like this:

```json
{
  "welcome": {
    "message": "Hello {name}",
    "backend": "ts"
  }
}
```

## ICU

intlc intentially breaks with the ICU spec where there's high value in doing so and output type safety can be guaranteed. For example, `select` and `plural` interpolation types don't require an `other` fallback case.

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
