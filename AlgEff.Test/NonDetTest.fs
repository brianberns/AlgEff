namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type NonDetLogEnv<'ret when 'ret : comparison>
    (createNonDetHandler : _ -> NonDetHandler<NonDetLogEnv<'ret>, 'ret>) as this =
    inherit Environment<'ret>()
    
    let handler =
        Handler.combine2
            (createNonDetHandler(this))
            (PureLogHandler(this))

    interface NonDetContext
    interface LogContext

    member _.Handler = handler

[<TestClass>]
type NonDetTest() =

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

    [<TestMethod>]
    member _.NonDet() =

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
            program () |> NonDetLogEnv(NonDetHandler.pickTrue).Handler.Run
        Assert.AreEqual(10, resultA)
        Assert.AreEqual(["x1: 15"; "x2: 5"; "x1 - x2: 10"], logA)

        let resultB, (Unit, logB) =
            program () |> NonDetLogEnv(NonDetHandler.pickMax).Handler.Run
        Assert.AreEqual(25, resultB)
        Assert.AreEqual(["x1: 30"; "x2: 5"; "x1 - x2: 25"], logB)

        let resultC =
            program ()
                |> NonDetLogEnv(NonDetHandler.pickAll).Handler.RunMany
                |> List.map fst
        Assert.AreEqual([10; 5; 25; 20], resultC)

    [<TestMethod>]
    member _.Pythagorean() =

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
                |> NonDetLogEnv(NonDetHandler.pickAll).Handler.RunMany
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

    [<TestMethod>]
    member _.NQueens() =

        let size = 4

        let isSafe (rowA, colA) (rowB, colB) =
            rowA <> rowB
                && colA <> colB
                && abs (rowA - rowB) <> abs (colA - colB)

        let allSafe positions position =
            positions
                |> Seq.forall (isSafe position)

        let rec fill positions =
            effect {
                let col = (positions |> List.length) + 1
                if col > size then
                    return positions
                else
                    let! row = chooseInt 1 size
                    let position = row, col
                    if allSafe positions position then
                        return! fill (position :: positions)
                    else
                        do! NonDet.fail
                        return []
            }

        let positions =
            fill []
                |> NonDetLogEnv(NonDetHandler.pickAll).Handler.RunMany
                |> List.map (fun (positions, _) -> List.rev positions)
        Assert.AreEqual(
            [
                [(2, 1); (4, 2); (1, 3); (3, 4)]
                [(3, 1); (1, 2); (4, 3); (2, 4)]
            ], positions)
