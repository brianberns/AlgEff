namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type PureConsoleLogContext<'res>(consoleInput) as this =
    inherit ConcreteContext<'res>()

    let handler =
        let consoleHandler = PureConsoleHandler(consoleInput, this)
        let logHandler = PureLogHandler(this)
        CombinedEffectHandler(consoleHandler, logHandler)
    
    interface ConsoleContext
    interface LogContext

    member __.Handler = handler

type PureStateContext<'state, 'res>(initial : 'state) as this =
    inherit ConcreteContext<'res>()

    let handler = PureStateHandler(initial, this)
    
    interface StateContext<'state>

    member __.Handler = handler

type PickTrueConcreteContext<'res>() as this =
    inherit ConcreteContext<'res>()
    
    let handler = PickTrue(this)

    interface NonDetContext

    member __.Handler = handler

type PickMaxConcreteContext<'res when 'res : comparison>() as this =
    inherit ConcreteContext<'res>()
    
    let handler = PickMax(this)

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
            PureConsoleLogContext(["John"]).Handler.Run(program)
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
            PureStateContext(1).Handler.Run(program)
        printfn "%A" state

    [<TestMethod>]
    member __.NonDet() =

        let program () =
            effect {
                let! x1 = NonDet.choose 15 30
                let! x2 = NonDet.choose 5 10
                return x1 - x2
            }

        let resultA, _ =
            program () |> PickTrueConcreteContext().Handler.Run
        Assert.AreEqual(10, resultA)

        let resultB, _ =
            program () |> PickMaxConcreteContext().Handler.Run
        Assert.AreEqual(25, resultB)
