# intlc

## Disclaimer

**Currently for internal use but feedback/external usage welcome**


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

---

Currently building against GHC 8.10.7.
