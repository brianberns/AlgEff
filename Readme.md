# AlgEff - Algebraic Effects for F#
## What are algebraic effects?
Algebraic effects provide a way to define and handle side-effects in  functional programming. This approach has several important benefits:
* Effects are defined in a purely functional way. This eliminates the danger of unexpected side-effects in otherwise pure functional code.
* Implementation of effects (via "handlers") is separate from the effects' definitions. You can handle a given effect type multiple different ways, depending on your needs. (E.g. one handler for unit tests, and another for production.)

In summary, you can think of algebraic effects as functional programming's answer to dependency injection in object-oriented programming. They solve a similar problem, but in a more functional way.
## Why use AlgEff?
AlgEff is one of the few algebraic effect systems for F#. It was inspired by a similar project called [Eff]([https://github.com/palladin/Eff](https://github.com/palladin/Eff)) and by Scala's ZIO.
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTI5MTMyMzU3MywtMTYyMTM5NzEzOF19
-->