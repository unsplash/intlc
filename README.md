# intlc

Compile ICU messages into code. Supports TypeScript and JSX. No runtime.

- **Compatible** - supports most common ICU syntax with some optional extras.
- **Typesafe** - embraces TypeScript output, taking advantage of unions to forgo the need for wildcards.
- **Lightweight** - no runtime, so no bundle or performance bloat. Just plain functions.
- **Fast** - compiles via a native binary. Most projects can expect everything to compile in under a second.
- **Unopinionated** - JSON/ICU in, code out. Structure your application around this in whichever way suits you.
- **Maintained** - in production at [unsplash.com](https://unsplash.com).

https://user-images.githubusercontent.com/6402443/194868749-23c86dd1-4996-4c60-a0b6-88685078fb38.mov

## Usage

```
Usage: intlc COMMAND
  Compile ICU messages into code.

Available options:
  -h,--help                Show this help text

Available commands:
  compile
  flatten
  lint
```

For example:

```console
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
    "description": "Welcome message",
    "backend": "ts"
  }
}
```

The description is optional and ignored by intlc. It can be used documentatively for developers and/or translators.

## Contributing

Check out `ARCHITECTURE.md`. Currently building against GHC 8.10.7.
