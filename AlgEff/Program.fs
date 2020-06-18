namespace AlgEff

module Program =

    let hypotenuse a b =
        effect {
            do! Log.writef "Side a: %g" a
            do! Log.writef "Side b: %g" b
            let c = sqrt <| (a*a + b*b)
            do! Log.writef "Side c: %g" c
            do! Console.writelnf "%g %g -> %g" a b c
            return c
        }

    let dump log =
        printfn ""
        printfn "Log contains %A entries:" (log |> List.length)
        for msg in log do
            printfn "   %s" msg

    (*
    let run a b =
        let c, log =
            hypotenuse a b
                |> handle
        dump log
        printfn "Final result: %g" c
    *)

    [<EntryPoint>]
    let main argv =
        let program = hypotenuse 3.0 4.0
        0
