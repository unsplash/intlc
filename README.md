# intlc

Compile ICU messages into code. Supports TypeScript and JSX. No runtime.

- **Compatible** - supports most common ICU syntax [with some optional extras](https://github.com/unsplash/intlc/wiki/ICU-syntax).
- **Typesafe** - embraces TypeScript output, taking advantage of unions to forgo the need for wildcards.
- **Lightweight** - no runtime, so no bundle or performance bloat. Just plain functions.
- **Fast** - compiles via a native binary. Most projects can expect everything to compile in under a second.
- **Unopinionated** - JSON/ICU in, code out. Structure your application around this in whichever way suits you.
- **Maintained** - in production at [unsplash.com](https://unsplash.com).

https://user-images.githubusercontent.com/6402443/194868749-23c86dd1-4996-4c60-a0b6-88685078fb38.mov

## CLI

Grab a binary from the releases page: https://github.com/unsplash/intlc/releases

```
Usage: intlc COMMAND
  Compile ICU messages into code.

Available options:
  -h,--help                Show this help text
  --version                Print version information

Available commands:
  compile
  flatten
  lint
  prettify
```

### Compiling

Take a JSON object of ICU messages, and a locale, and output TypeScript to stdout.

```console
$ cat translations.json
{"welcome":{"message": "Hello {name}"}}
$ intlc compile translations.json -l en-US > translations.ts
$ cat translations.ts
export const welcome: (x: { name: string }) => string = x => `Hello ${x.name}`
```

Check out an example project integration in our wiki: https://github.com/unsplash/intlc/wiki/Example-project-integration

### Flattening

Hoist selectors up as much as possible. This is often preferred by translators.

```console
$ cat translations.json
{"openSource":{"message": "Open source at {company} is {company, select, Unsplash {encouraged!} other {unknown}}"}}
$ intlc flatten --minify translations.json
{"openSource":{"message":"{company, select, Unsplash {Open source at {company} is encouraged!} other {Open source at {company} is unknown}}"}}
```

### Linting

Lint against suboptimal use of ICU syntax.

```console
$ cat translations.json
{"welcome":{"message": "Hello {name, select, other {{name}}}"}}
$ intlc lint translation.json
translations.json:1:32:
  |
1 | {"welcome":{"message": "Hello {name, select, other {{name}}}"}}
  |                                ^^^^
redundant-select: Select named `name` is redundant as it only contains a wildcard.

Learn more: https://github.com/unsplash/intlc/wiki/Lint-rules-reference#redundant-select
```

A reference for lint rules can be found in our wiki: https://github.com/unsplash/intlc/wiki/Lint-rules-reference

### Formatting

Pretty-print an ICU message. Useful for inspecting larger messages such as flattened ones.

```console
$ cat translations.json
{"tagline": {"message":"{hasTags, boolean, true {{type, select, overLimit {{upperLimit, number}+ best free {formattedListOfTags} photos on Unsplash} belowLimit {{photoTotal, number} best free {formattedListOfTags} photos on Unsplash}}} false {{type, select, overLimit {{upperLimit, number}+ best free photos on Unsplash} belowLimit {{photoTotal, number} best free photos on Unsplash}}}}"}}
$ intlc prettify $(cat translations.json | jq -r .tagline.message)
{hasTags, boolean,
  true {{type, select,
    overLimit {{upperLimit, number}+ best free {formattedListOfTags} photos on Unsplash}
    belowLimit {{photoTotal, number} best free {formattedListOfTags} photos on Unsplash}
  }}
  false {{type, select,
    overLimit {{upperLimit, number}+ best free photos on Unsplash}
    belowLimit {{photoTotal, number} best free photos on Unsplash}
  }}
}

```

## Schema

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

At present, the following backends (compilation targets) are supported:

- TypeScript (`ts`, default)
- TypeScript/React (`tsx`)

The description is optional and ignored by intlc. It can be used documentatively for developers and/or translators.

## Contributing

Check out `ARCHITECTURE.md`.

Currently building against GHC 9.6.4. A Nix flake is included with all necessary dependencies.
