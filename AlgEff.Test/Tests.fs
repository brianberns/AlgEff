namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type GreetContext<'ret>(consoleInput) as this =
    inherit ConcreteContext<'ret>()

    let handler =
        Handler.combine3
            (PureConsoleHandler(consoleInput, this))
            (PureLogHandler(this))
            (PureStateHandler("", this))
    
    interface ConsoleContext
    interface LogContext
    interface StateContext<string>

    member __.Handler = handler

type PureStateContext<'state, 'ret>(initial : 'state) as this =
    inherit ConcreteContext<'ret>()

    let handler = PureStateHandler(initial, this)
    
    interface StateContext<'state>

    member __.Handler = handler

type PickTrueConcreteContext<'ret>() as this =
    inherit ConcreteContext<'ret>()
    
    let handler = PickTrue(this)

    interface NonDetContext

    member __.Handler = handler

type PickMaxConcreteContext<'ret when 'ret : comparison>() as this =
    inherit ConcreteContext<'ret>()
    
    let handler = PickMax(this)

    interface NonDetContext

    member __.Handler = handler

type PickAllConcreteContext<'ret when 'ret : comparison>() as this =
    inherit ConcreteContext<'ret>()
    
    let handler = PickAll(this)

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

        let resultA, Unit =
            program () |> PickTrueConcreteContext().Handler.Run

        Assert.AreEqual(10, resultA)

        let resultB, Unit =
            program () |> PickMaxConcreteContext().Handler.Run
        Assert.AreEqual(25, resultB)

        let resultC =
            program () |> PickAllConcreteContext().Handler.RunMany
        Assert.AreEqual([10, Unit; 5, Unit; 25, Unit; 20, Unit], resultC)

    [<TestMethod>]
    member __.Pythagorean() =

        let rec chooseInt m n =
            effect {
                if m > n then
                    do! NonDet.fail
                    return -1
                else
                    let! flag = NonDet.decide
                    if flag then return m
                    else return! chooseInt (m + 1) n
            }

        let perfectSqrt x =
            let root = float x |> sqrt |> int
            if root * root = x then
                Some root
            else None

        let pythagorean m n =
            effect {
                let! a = chooseInt m (n - 1)
                let! b = chooseInt (a + 1) n
                match perfectSqrt (a * a + b * b) with
                    | Some root ->
                        return (a, b, root)
                    | None ->
                        do! NonDet.fail
                        return (-1, -1, -1)
            }

        let triples =
            pythagorean 4 15
                |> PickAllConcreteContext().Handler.RunMany
                |> List.map fst
        Assert.AreEqual(
            [
                5, 12, 13
                6,  8, 10
                8, 15, 17
                9, 12, 15
            ],
            triples
        )
