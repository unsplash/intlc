# intlc

For the time being the CLI takes a single translation JSON via stdin. To play with this in dev, run:

```
$ echo '{ "title": "Unsplash", "greeting": "Hello <bold>{name}</bold>, {age, number}!" }' | cabal run -- intlc
```

Currently building against GHC 8.10.7.
