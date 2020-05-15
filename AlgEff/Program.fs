namespace AlgEff

module Program =

    open Effect

    let hypotenuse a b =
        effect {
            do! logf "Side a: %g" a
            do! logf "Side b: %g" b
            let c = sqrt <| (a*a + b*b)
            do! logf "Side c: %g" c
            return c
        }

    let dump log =
        printfn ""
        printfn "Log contains %A entries:" (log |> List.length)
        for msg in log do
            printfn "   %s" msg

    let run a b =
        let c, log =
            hypotenuse a b
                |> handle
        dump log
        printfn "Final result: %g" c

    [<EntryPoint>]
    let main argv =
        run 3.0 4.0
        run 5.0 12.0
        0
