# intlc

For the time being the CLI takes a single translation input via stdin. To play with this in dev, run:

```
$ echo '{ "title": "Unsplash", "greeting": "Hello {name}, {age, number}!" }' | cabal run -- intlc
```

Currently building against GHC 8.10.7.
