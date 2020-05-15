namespace AlgEff

module Program =

    open Effect

    let hypotenuse a b =
        effect {
            do! logf "Side a: %A" a
            do! logf "Side b: %A" b
            let c = sqrt <| (a*a + b*b)
            do! Effect.logf "Side c: %A" c
            return c
        }

    let dump log =
        printfn ""
        printfn "Log contains %A entries:" (log |> List.length)
        for msg in log |> List.rev do
            printfn "   %s" msg

    let run a b =
        let c, log =
            hypotenuse a b
                |> handle
        dump log
        printfn "Final result: %A" c

    [<EntryPoint>]
    let main argv =
        run 3.0 4.0
        run 5.0 12.0
        0
