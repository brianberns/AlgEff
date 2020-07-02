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
Let's write a simple effectful program that interacts with the user via a console and then logs the result:
```fsharp
let program =
    effect {
        do! Console.writeln "What is your name?"
        let! name = Console.readln
        do! Console.writelnf "Hello %s" name
        do! Log.writef "Name is %s" name
        return name
    }
```
The type of this value is:
```fsharp
Program<'ctx, string when 'ctx :> LogContext and 'ctx :> ConsoleContext>
```
The first type parameter (`'ctx`) indicates that the program requires handlers for both logging and console effects, and the second one (`string`) indicates that the program returns a string. It's important to understand that this program doesn't actually **do** anything until it's executed. The `program` value itself is purely functional -- no side-effects occurred in the process of creating it.

In order to run this program (and potentially cause actual side-effects), we must define an environment that satisfies the program's requirements:
```fsharp
type ProgramEnv<'ret>() as this =
    inherit Environment<'ret>()

    let handler =
        Handler.combine2
            (PureLogHandler(this))
            (ActualConsoleHandler(this))
    
    interface ConsoleContext
    interface LogContext

    member __.Handler = handler
```
The important thing to note here is that our environment contains both a log handler (``)

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
eyJoaXN0b3J5IjpbMzExMjAwMzQ3LDE2NzkyOTg1OTAsMzU2Mz
M4NDM5LC0xNjIxMzk3MTM4XX0=
-->