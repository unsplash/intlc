# Architecture

The project is split into two parts, `cli` and `lib`.

## CLI

The CLI portion is fairly small and handles taking user input and producing output. Thinking of a “functional core, imperative shell”, this is the shell. The data flow starts here, moves into the library, and ends up back here again.

## Library

The library is the pure functional core. It takes care of parsing and compiling, taking anticipated valid JSON input and outputting, if all goes well, a string representing code in the format of the requested backend.

### Parsing

The ICU message parser is a [recursive descent parser](https://en.wikipedia.org/wiki/Recursive_descent_parser) written with [Megaparsec](https://hackage.haskell.org/package/megaparsec). Recursive descent essentially means for the mental model that we have a recursive tree of parsers that will each try in turn to match on some amount of the text on the left-hand side of our string, progressively parsing from left-to-right until we reach the end.

For example, given an ICU message `hello <bold>{name}</bold>`, we’d first try parsing for an interpolation or tag, and failing that would parse plaintext, which we’d do until we encountered a reason to stop, in this case the tag opening character. We’ve stored "hello " as plaintext and will now continue along, this time succeeding in parsing the tag. We’ll now recursively parse inside the bounds of the tag, reusing the same top-level parser we were just using, this time parsing an interpolation. Having done this we’ve consumed the entire input string and have successfully parsed a recursive list of nodes making up our AST.

JSON parsing is offloaded to [aeson](https://hackage.haskell.org/package/aeson).

### Compilation

Should parsing succeed the ICU AST will be passed along to the relevant backend compiler, which takes care of compilation and potentially some validation. The compilers generally exist in isolation from one-another and can be implemented in completely different ways; our JavaScript and TypeScript compilers are a special case in which they know about each other for code reuse due to the runtime representations being identical. This flexibility will make it easier to implement new backends for distinct languages.

The compilers take the ICU AST and recursively compile each node into an output string, which is all concatenated together to form our output code. This is in a sense the exact inverse of the parsing we did before. An implementation detail of the compilers we have so far is that for comprehensibility they first convert the ICU AST into a minimal target-language AST, before compiling against this intermediary AST.
