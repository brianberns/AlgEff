namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type PureConsoleLogContext<'res>(consoleInput) as this =
    inherit ConcreteContext<'res>()

    let handler =
        let consoleHandler = this |> ConsoleHandler.createPure consoleInput
        let logHandler = this |> LogHandler.createPure
        EffectHandler.combine consoleHandler logHandler
    
    interface ConsoleContext
    interface LogContext

    member __.Handler = handler

type PureStateContext<'state, 'res>(initial : 'state) as this =
    inherit ConcreteContext<'res>()

    let handler = this |> StateHandler.createPure initial
    
    interface StateContext<'state>

    member __.Handler = handler

type NonDetConcreteContext<'res>(getHandler) as this =
    inherit ConcreteContext<'res>()
    
    let handler = this |> getHandler

    interface NonDetContext

    member __.Handler = handler

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member __.Greet() =

        let program =
            effect {
                do! Console.writeln "What is your name?"
                let! name = Console.readln
                do! Console.writelnf "Hello %s" name
                do! Log.writef "Name is %s" name
                return name
            }

        let name, (console, log) =
            PureConsoleLogContext(["John"]).Handler
                |> EffectHandler.run program
        Assert.AreEqual("John", name)
        Assert.AreEqual(List.empty<string>, console.Input)
        Assert.AreEqual(["What is your name?"; "John"; "Hello John"], console.Output)
        Assert.AreEqual(["Name is John"], log)

    [<TestMethod>]
    member __.State() =

        let program =
            effect {
                let! x = State.get
                do! State.put (x + 1)
                let! y = State.get
                do! State.put (y + y)
                return! State.get
            }

        let state =
            PureStateContext(1).Handler
                |> EffectHandler.run program
        printfn "%A" state

    [<TestMethod>]
    member __.NonDet() =

        let program =
            effect {
                let! x1 = NonDet.choose 15 30
                let! x2 = NonDet.choose 5 10
                return x1 - x2
            }

        let result, () =
            NonDetConcreteContext(NonDetHandler.pickTrue).Handler
                |> EffectHandler.run program
        Assert.AreEqual(10, result)

        (*
        let result, () =
            NonDetConcreteContext(NonDetHandler.pickMax).Handler
                |> EffectHandler.run program
        Assert.AreEqual(25, result)
        *)
