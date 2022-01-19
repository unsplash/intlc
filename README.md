# intlc

For the time being the CLI has the following interface:

```
intlc filepath (-l|--locale ARG)
```

For example:

```
$ cabal run -- intlc ./translations.json -l en-US
```

Currently building against GHC 8.10.7.
