namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type GreetContext<'res>(consoleInput) as this =
    inherit ConcreteContext<'res>()

    let handler =
        Handler.combine3
            (PureConsoleHandler(consoleInput, this))
            (PureLogHandler(this))
            (PureStateHandler("", this))
    
    interface ConsoleContext
    interface LogContext
    interface StateContext<string>

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
                do! State.put name
                let! state = State.get
                return state.Length
            }

        let result, (console, log, state) =
            GreetContext(["John"]).Handler.Run(program)
        Assert.AreEqual(4, result)
        Assert.AreEqual(List.empty<string>, console.Input)
        Assert.AreEqual(["What is your name?"; "John"; "Hello John"], console.Output)
        Assert.AreEqual(["Name is John"], log)
        Assert.AreEqual("John", state)

    [<TestMethod>]
    member __.State() =

        let program =
            effect {
                let! x = State.get
                do! State.put (x + 1)
                let! y = State.get
                do! State.put (y + y)
                let! z = State.get
                return z.ToString()
            }

        let result, state =
            PureStateContext(1).Handler.Run(program)
        Assert.AreEqual("4", result)
        Assert.AreEqual(4, state)

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
