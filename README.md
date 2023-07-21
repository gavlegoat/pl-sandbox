# PL Sandbox

This is a repository for me to play around with programming languages
implementation ideas. At the moment it is a work-in-progress compiler for a
small functional language.

## Language

The input language is (roughly) as follows. Note that '|' and '||' are written
with quotes only to distinguish them from the | used to separate rules in the
grammar.

```
<program> ::= <binding> [ ;; <binding> ]+

<binding> ::= <type_binding> | <value_binding>

<type_binding> ::= data $constr = <data_alt> [ '|' <data_alt> ]+

<data_alt> ::= $constr [ <type> ]*

<type> ::= Int | String | Bool
         | <type> -> <type>
         | $constr <type>
         | ( <type> )

<value_binding> ::= $id [ $id ]+ = <expr>

<expr> ::= $id | $constr | <constant>
         | <expr> <binop> <expr> | <unop> <expr>
         | <expr> <expr>
         | if <expr> then <expr> else <expr>
         | case <expr> of [ <alt> ; ]+
         | let $id = <expr> in <expr>
         | \ $id -> <expr>

<alt> ::= <pattern> -> <expr>

<pattern> ::= $constr [ <pattern> ]*
            | $id | <constant>
            | ( <pattern> )

<binop> ::= + | - | * | / | < | <= | == | /= | >= | > | && | '||'

<unop> ::= ! | -

<constant> ::= $int | $string

$constr ~= [A-Z] [A-Za-z0-9_]*

$id ~= [a-z] [A-Za-z0-9_]*

$int ~= [0-9]+
```

## Structure

The compiler follows a fairly standard pipeline with many details being heavily
influenced by GHC. The code is laid out in the directory structure as follows:

- `Main.hs` defines the top-level executable.
- `Type.hs` defines the types used by programs.
- `Core.hs` is our intermediate language (following GHC).
- `Cmm.hs` is a secondary intermediate language closer to the target language
  (again, folling GHC). The Cmm langage is shared by both the strict and lazy
  backends.
- `Frontend` contains everything up to the generation of core code.
  `Frontend.hs` serves as a wrapper for all of this functionality.
  + `Source.hs` contains AST definitions for the input language.
  + `Lexer.x` and `Parser.y` generate a source AST from textual input.
  + `Checks.hs` runs semantic checks (not including typechecking).
  + `Typecheck.hs` performs type inference and annotates the source AST with
    type information.
  + `Desugar.hs` converts the typed source code to core code.
- `StrictBackend` holds code needed for generating a strictly-evaluating
  version of the input code.
- `LazyBackend` generates a lazy version of the input code.
- `TargetGen` converts Cmm code to whatever output format is being targeted.
