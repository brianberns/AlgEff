namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

/// Environment that provides console, log, and state handlers.
type GreetEnv<'ret>(consoleInput) as this =
    inherit Environment<'ret>()

    let handler =
        Handler.combine3
            (PureConsoleHandler(consoleInput, this))
            (PureLogHandler(this))
            (PureStateHandler("", this))
    
    interface ConsoleContext
    interface LogContext
    interface StateContext<string>

    member _.Handler = handler

[<TestClass>]
type GreetTest() =

    [<TestMethod>]
    member _.Greet() =

            // require console, log, and state handlers
        let program =
            effect {
                do! Console.writeln "What is your name?"
                let! name = Console.readln
                do! Console.writelnf "Hello %s" name
                do! Log.writef "Name is %s" name
                do! State.put name
                let! state = State.get
                return state.Length
            }

        let result, (console, log, state) =
            GreetEnv(["John"]).Handler.Run(program)
        Assert.AreEqual<int>(4, result)
        Assert.AreEqual(List.empty<string>, console.Input)
        Assert.AreEqual(["What is your name?"; "John"; "Hello John"], console.Output)
        Assert.AreEqual(["Name is John"], log)
        Assert.AreEqual<string>("John", state)
