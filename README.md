# intlc

For the time being the CLI takes a single translation input via stdin. To play with this in dev, run:

```
$ echo "Hello, %%FORENAME%% %%SURNAME%%!" | cabal run -- intlc
```

Currently building against GHC 8.10.7.
