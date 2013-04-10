* Lexer rules.
* Nested lexer rules using `push` and `pop` action. This requires that there
  are tokens that can be used to detect lexer nesting.
* Subset of lexers rules. A token once parsed can further be analysed using
  a subset of lexer rules, thus the properties of parent token can be
  overriden by defining properties for sub-tokens.
* Skip rules.
