# ⚡parse-bolt⚡ ...and Kotlin

## Status
Discontinued for now in favour of LALRPOP. I mistakenly thought LALRPOP was LALR(1), when infact it was LR(1). LALRPOP is exactly what I need at the moment.

Changing direction a bit (and maybe project name will change later). Currently focused on a Kotlin Parser, plus Parser combinators for building other Parsers. The Kotlin grammar is battle testing the Parser combinators.

This project makes use of parser combinators to define the grammar of your language in the source.

Not production ready. Works, but not heavily tested!

For example usage see:

- [```test/backtracking.rs```](https://github.com/clinuxrulz/parse-bolt/blob/main/src/test/backtracking.rs)
- [```kotlin/lexer.rs```](https://github.com/clinuxrulz/parse-bolt/blob/main/src/kotlin/lexer.rs)
- [```kotlin/parser.rs```](https://github.com/clinuxrulz/parse-bolt/blob/main/src/kotlin/parser.rs)

This library makes use of parser combinators. Functions that combine smaller parsers together to make bigger parsers. Here is a table of some of those combinators:

| Combinator                      | Description                                                                                                                   |
| ------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| ```Parser::any()```             | Matches any single token.                                                                                                     |
| ```Parser::empty()```           | Matches nothing and consumes no tokens.                                                                                       |
| ```Parser::eof()```             | Matches end of file.                                                                                                          |
| ```Parser::satisfy(pred)```     | Matches any token where ```pred(token)``` returns true.                                                                       |
| ```Parser::match(t)```          | Matches token ```t```.                                                                                                        |
| ```Parser::match_string(str)``` | Matches a string.                                                                                                             |
| ```Parser::unimplemented()```   | A temporary placeholder for a WIP implementation of a smaller parser while implementing the larger parser. (it always panics) |
| ```Parser::lazy(unbox)```       | Constructs a lazyly constructed parser. This is useful for recursive grammar. You can use it to prevent infinite recursion when constructing parser. |
| ```Parser.map(f)```             | Modifies the final result of the parser with function ```f```.                                                                |
| ```Parser.map_to(a)```          | Ignores the final result of the parser and returns ```a```.                                                                   |
| ```Parser.or_else(p)```         | Run two parsers in parallel if ```self``` parser passes, then use it. Otherwise use parser ```p```.                           |
| ```Parser.seq2(p)```            | Sequence two parsers together as one and combind their results in a tuple.                                                    |
| ```Parser.seq_left(p)```        | Sequence two parsers together and returns the result of the first parser.                                                     |
| ```Parser.seq_right(p)```       | Sequence two parsers together and returns the result of the second parser.                                                    |
| ```Parser.optional()```         | The thing being parsed is optional. Return a successful ```Some(thing)``` if found, otherwise return a successful ```None```. |
| ```Parser.zero_or_more_vec()``` | Run the ```self``` parser as many times as possible while accumulating the result into a ```Vec<>```. Also backtracking will cause smaller sized Vecs to be tried out. |
| ```Parser.one_or_more_vec()```  | Same as ```Parser.zero_or_more()```, but must have atleast one result to be successful.                                       |
| ```Parser.return_string()```    | Ignores the result of the parser and returns a string formed from the range of tokens matched.                                |
