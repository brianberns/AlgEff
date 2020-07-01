namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type PickTrueLogContext<'ret>() as this =
    inherit ContextSatisfier<'ret>()
    
    let handler =
        Handler.combine2
            (PickTrue(this))
            (PureLogHandler(this))

    interface NonDetContext
    interface LogContext

    member __.Handler = handler

type PickMaxLogContext<'ret when 'ret : comparison>() as this =
    inherit ContextSatisfier<'ret>()
    
    let handler =
        Handler.combine2
            (PickMax(this))
            (PureLogHandler(this))

    interface NonDetContext
    interface LogContext

    member __.Handler = handler

type PickAllLogContext<'ret>() as this =
    inherit ContextSatisfier<'ret>()
    
    let handler =
        Handler.combine2
            (PickAll(this))
            (PureLogHandler(this))

    interface NonDetContext
    interface LogContext

    member __.Handler = handler

[<TestClass>]
type NonDetTest() =

    [<TestMethod>]
    member __.NonDet() =

        let program () =
            effect {
                let! x1 = NonDet.choose 15 30
                do! Log.writef "x1: %A" x1
                let! x2 = NonDet.choose 5 10
                do! Log.writef "x2: %A" x2
                do! Log.writef "x1 - x2: %A" <| x1 - x2
                return x1 - x2
            }

        let resultA, (Unit, logA) =
            program () |> PickTrueLogContext().Handler.Run
        Assert.AreEqual(10, resultA)
        Assert.AreEqual(["x1: 15"; "x2: 5"; "x1 - x2: 10"], logA)

        let resultB, (Unit, logB) =
            program () |> PickMaxLogContext().Handler.Run
        Assert.AreEqual(25, resultB)
        Assert.AreEqual(["x1: 30"; "x2: 5"; "x1 - x2: 25"], logB)

        let resultC =
            program ()
                |> PickAllLogContext().Handler.RunMany
                |> List.map fst
        Assert.AreEqual([10; 5; 25; 20], resultC)

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
                |> PickAllLogContext().Handler.RunMany
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
