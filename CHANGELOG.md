# Changelog

This project adheres to semantic versioning.

## 0.8.3 (2024-04-24)

Fixed dynamic linking in macos-aarch64 binary.

## 0.8.2 (2024-04-23)

Allowed "✓" character in internal linter.

## 0.8.1 (2023-02-20)

Fixed flattening in the presence of callback tags.

## 0.8.0 (2023-01-23)

Added `--indent` option to the flatten and prettify commands. This enables explicit configuration of formatting indentation either as tabs or as a number of spaces. The option is ignored in the presence of `--minify` where applicable.

Shifted everything from the "internal" binary into the main binary behind hidden flags.

Improved CI binary naming, clarifying in particular which architecture they're built against.

Added a `--version` option.

Added a changelog. Everything prior to this release may not be perfectly accurate.

## 0.7.0 (2023-01-13)

Prettify JSON by default. A `--minify` flag has been added to retain the old behaviour.

## 0.6.1 (2022-12-08)

Fixed linking of binaries produced in CI.

## 0.6.0 (2022-11-21)

Added ICU prettify command.

Redundant interpolations can now be parsed. They are linted against instead of being wholly disallowed. Further lint rules have been added, and linting output is now substantially better.

## 0.5.0 (2022-07-23)

Added linting.

## 0.4.1 (2022-07-05)

Fixed parsing of escaped ICU leaking across JSON messages.

## 0.4.0 (2022-07-01)

Improved error reporting to now eagerly report as many errors as possible before halting.

Fixed TypeScript output producing a type error because tsc tries to be too smart.

Fixed formatting of plurals in flattened output.

Added an "internal" binary intended for Unsplash, featuring lint rules for specific use cases and plural expansion.

## 0.3.2 (2022-04-08)

Fixed `boolean` interpolations not being flattened.

## 0.3.1 (2022-04-05)

Fixed codegen of interpolations inside `boolean` interpolations.

## 0.3.0 (2022-03-17)

Added a `boolean` type.

Typechecking of output TypeScript code is now offloaded to downstream tsc.

Improved JSON key validation.

Compiled output is now alphabetical.

Fixed various codegen issues, and parsing of `#` in `select` interpolations.

## 0.2.2 (2022-03-04)

Fixed description key not being preserved when flattening.

## 0.2.1 (2022-02-03)

ICU parsing is now stricter, JSON keys are somewhat validated, and React import statements are conditional.

Fixed parsing of nested `#` plural interpolations.

intlc is now MIT licensed.

## 0.2.0 (2022-01-26)

Added flattening and `--help`.

## 0.1.1 (2022-01-25)

Fixed React import casing.

## 0.1.0 (2022-01-24)

Hello world!
