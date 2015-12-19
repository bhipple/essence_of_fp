# Essence of Functional Programming
Notes taken while reading Philip Wadler's paper on Monads in Haskell.

We first use generic moands to build up 7 different lambda calculus interpreters, with only slight changes to the monad definitions.

We then demonstrate that we can convert the interpreter from call-by-value to call-by-name, again with only a small monad change.

Next, we explore Continuation Passing Style and how it relates to Monadic style.

Finally, we discuss how Monads are used in the Haskell compiler for type inference and error handling, to great effect, with some discussion on past and present academic literature on monads and category theory in language design.
