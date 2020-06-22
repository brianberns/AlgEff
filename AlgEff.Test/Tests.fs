namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type PureConsoleLogContext<'res>(input) as this =
    inherit ConcreteContext<'res>()

    let handler =
        let consoleHandler = this |> ConsoleHandler.createPure input
        let logHandler = this |> LogHandler.createPure
        EffectHandler.combine consoleHandler logHandler
    
    interface ConsoleContext
    interface LogContext

    member __.Run(program) =
        handler |> EffectHandler.run program

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.Greet () =

        let program =
            effect {
                do! Console.writeln "What is your name?"
                let! name = Console.readln
                do! Console.writelnf "Hello %s" name
                do! Log.writef "Name is %s" name
                return name
            }

        let name, (console, log) =
            program |> PureConsoleLogContext(["John"]).Run
        Assert.AreEqual("John", name)
        Assert.AreEqual(List.empty<string>, console.Input)
        Assert.AreEqual(["What is your name?"; "John"; "Hello John"], console.Output)
        Assert.AreEqual(["Name is John"], log)
