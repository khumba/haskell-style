# Haskell Style Guide

This is [Khumba's](http://khumba.net) style guide for Haskell source code.

*Legalese: Copyright 2015 Bryan Gardiner.  Licensed under the
[Creative Commons Attribution-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-sa/4.0/).*

## Formatting

This section contains style guidelines for the visual layout of code.

### Encoding

Haskell source files are UTF-8, so use printable non-ASCII characters freely in
strings (use escaped syntax for non-printable characters).  Don't use the
Unicode equivalents of `->`, `=>`, `::`, `forall`, etc.

### Line length

Maximum 100 characters, but feel free to wrap earlier than necessary.
Exceptions are things that shouldn't be broken, for example long paths or URLs.

### Whitespace

Put one blank line after the `module` declaration, around the entire import
group, and between top-level declarations.  Also use a blank lines between
definitions if at least one of the definitions takes more than a single line
(e.g. because of including a type declaration or comments) in `let`, `where`,
`class` and `instance`, etc.  (Optional for record fields.)

Except at the start or end of a line, put one space around binary operators; do
not align operators in the middle of lines:

    let abacus = 3
        cake   = 1  -- No.

For end-of-line comments, put two spaces before the start of the comment.

Remove trailing whitespace.

### Breaking expressions across multiple lines

Breaking large statements across multiple lines can be necessary to meet the
100-character limit and also can improve readability.  Prefer to break at outer
expressions rather than inner ones, for example for the following line,

    someVariable = someAction . foo =<< someOtherAction
                  A            C       B

breaking at `A` would be best, and `B` would be second-best, and `C` would be
bad and misleading, although the following would be okay because there is a
natural flow from right to left:

    someVariable =
      someAction .
      foo =<<
      someOtherAction

Should you break a line before or after an operator?  That depends on the
operator.  Unless otherwise specified, operators should go at the end of the
line.  This includes `::` and `=`, which makes it convenient to search for
e.g. `"someBinding ::"`.  Operators which when wrapped should go at the start of
a line are:

- The function type `->` and class constraint specifier `=>`.
- List and record value separator `,` and closing characters `]` and `}`.
- `)`, only in a module export list.
- `|` between data constructors in a data declaration.

Function types can be wrapped like

    divideM ::
      (Num a, Show a, Monad m) => a -> a -> m a

or

    divideM :: (Num a, Show a, Monad m)
            => a  -- ^ Divisor.
            -> a  -- ^ Quotient.
            -> m a

or, if single types are really long or if you want to put any parameter
documentation onto its own line(s), then use:

    divideM ::
         (Num a, Show a, Monad m)
      => a  -- ^ This is the divisor.
      -> a
         -- ^ This is the quotient.  Multi-line comments get to start on their
         -- own line.
      -> m a

When wrapping function calls, indent subsequent lines to the level of the first
function argument:

    someFunction foo bar baz
                 quux
                 quuux

or:

    someFunction
      foo bar baz
      quux
      quuux

### Indentation

Use tabs, not spaces.  When line breaking to start an indented expression after
a function arrow `->`, `case ... of`, `do`, `let ... in`, `where`, etc.,
increase the indentation by two spaces.  When writing an `if` or `let` within a
`do`, indent the `true`, `false`, and `in` parts two spaces.

### The application operator

`$` versus parentheses: prefer the shorter one for your use case, choosing `$`
if the lengths are equal (which they will be in the common case).  Parentheses
may be better in other cases, for example:

    foo (bar a) (bar b)

versus:

    foo (bar a) $ bar b

Instead of writing:

    foo . bar . baz $ quux x

prefer this:

    foo $ bar $ baz $ quux x

### Lists

Single-line lists should be formatted like `[foo, bar, baz]` without spaces on
the inside of the brackets.  Multi-line lists can use either

    [foo, bar,
     baz, quux, quuux]

or

    [ foo
    , bar
    , baz
    , quux
    , quuux
    ]

formats.  The former is preferred for short lists of fixed length since it
doesn't require an extra line for the closing bracket, and the latter is
preferred for lists that may grow gradually since it makes diffs look nicer.

### Tuples

If a tuple `(foo, bar, baz)` doesn't fit on a single line, it should be wrapped
in the first multi-line list style above, since tuples also have fixed length.

### Data types

Data types with a single data constructor using record syntax are formatted
like:

    data Foo = Foo
      { fooA :: A
      , fooB :: B
      } deriving (Eq, Ord, Show)

Data types with multiple constructors and without field names are formatted
like:

    data Choice =
        Yes
      | No
      | FileNotFound
        -- ^ The obvious choice.

## Language use

This section contains style guidelines for the use of Haskell language features.

### Module imports and exports

[Import modules properly:](https://wiki.haskell.org/Import_modules_properly) do
not import the entire contents of another package's module unqualified.

Sort all imports alphabetically by module name.  When a module is imported in
both qualified and unqualified form, put the qualified form first.

Single- or few-letter qualified module names (`import qualified
Data.ByteString.Lazy as BL`) are fine as long as you are consistent with your
abbreviation usage in your project.

Always give modules export lists.  Either the complete module declaration should
fit on one line:

    module My.Awesome.Module (foo, bar, baz, quux, quuux) where

or it should be wrapped as follows:

    module My.Awesome.Module (
      foo, bar, baz (..),
      quux (a, b, c),
      quuux (
        m, n, o,
        p, q, r
      ),
      ) where

with commas at the end of lines that allow them.  Outer exports can always be
followed by a comma, but a final trailing comma is not allowed in a sub-export.

### Type signatures

All top-level bindings should include type signatures.  Type signatures are not
required on `let` or `where` bindings.

When writing a type with class constraints (`=>`), or a function type (`->`),
the type should either fit on a single line, or else have each `=> ...` or
`-> ...` section on its line.

### Data types

Avoid using record syntax for data types with multiple data constructors, since
the record accessors are partial functions.  Factor the records out into
separate data types, or if all the records' fields are the same, use one data
constructor with an enum inside.

### Typeclasses

Typeclasses are amazing, but don't write classes that are not expected to take
advantage of polymorphism from multiple instances.  Do not write orphan
instances.

### Haddock comments

Modules, and all bindings exported from modules, should have Haddock
documentation attached to them.  Use `-- |` on the line before the declaration,
except when including the comment on the same line as the declaration, in which
case `-- ^` is needed, or when documenting function parameter types or record
fields.  Don't use `{- | -}` or `{- ^ -}`.

    -- | This function does nothing.
    nop :: IO ()
    nop x =
      let -- | Actually unused!
          y = f x
      in return ()

    -- | This function does something.
    doSomething ::
         String
         -- ^ Separate-line comment.
      -> String
         -- ^ Another separate-line comment.
      -> Int  -- ^ Inline comment.
      -> ()

    data MyRecord = MyRecord
      { earthAgeMillis :: Long
        -- ^ Age of the earth in milliseconds.
      , favouriteColour :: Colour
        -- ^ My colour of choice.
      , dirtyBool :: Bool  -- ^ Depends on the weather.
      }

The `--` for a Haddock comment should be aligned with the identifier to which it
belongs.

Haddock comments should use complete sentences, although the first sentence of a
comment may be a sentence fragment referring to the thing being documented.

Use hyperlinks liberally.  If you're going to `@wrap@` a name, you might as well
`'link'` it.

### Redundancy, lint, warnings

Avoid reduncancy, refactoring when appropriate.  This also applies to syntactic
elements: clean up unused imports, remove unnecessary `do`s, and in general
listen to the suggestings of HLint.  Code should compile warning-free with `-W`
enabled.
