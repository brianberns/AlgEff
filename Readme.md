# AlgEff - Algebraic Effects for F#
## What are algebraic effects?
Algebraic effects provide a way to define and handle side-effects in  functional programming. This approach has several important benefits:
* Effects are defined in a purely functional way. This eliminates the danger of unexpected side-effects in otherwise pure functional code.
* Implementation of effects (via "handlers") is separate from the effects' definitions. You can handle a given effect type multiple different ways, depending on your needs. (For example, you can use one handler for unit tests, and another for production.)

In summary, you can think of algebraic effects as functional programming's answer to dependency injection in object-oriented programming. They solve a similar problem, but in a more functional way.
## Why use AlgEff?
AlgEff is one of the few algebraic effect systems for F#. It was inspired by a similar F# library called [Eff](https://github.com/palladin/Eff) and by [Scala's ZIO](https://zio.dev/). Reasons to use AlgEff:
* Effects are easy to define.
* Handlers are easy to define.
* Programs that use effects and handlers are easy to write.
* Strong typing reduces the possibility of unhandled effects.
## A simple example

## Defining an effect
One of the simplest effects is for writing strings to a log. This effect is defined as follows:
```fsharp
/// Logs the given string.
type LogEffect<'next>(str : string, cont : unit -> 'next) =
    inherit Effect<'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        LogEffect(str, cont >> f) :> _

    /// String to log.
    member __.String = str

    /// Continuation to next effect.
    member __.Cont = cont
```
There are several important things to notice 
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTE1NjE0MDY4MjMsMTY3OTI5ODU5MCwzNT
YzMzg0MzksLTE2MjEzOTcxMzhdfQ==
-->