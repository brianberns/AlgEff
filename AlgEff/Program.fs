namespace AlgEff

module Program =

    [<EntryPoint>]
    let main argv =

        let mySqrt x =
            effect {
                do! Effect.logf "Input: %A" x
                let result = sqrt x
                do! Effect.logf "Result: %A" result
                return result
            }

        let run x =

            let result, log =
                mySqrt x
                    |> Effect.handle

            printfn ""
            printfn "Log contains %A entries:" log.Length
            for msg in log |> List.rev do
                printfn "   %s" msg
            printfn "Final result: %A" result

        run 2.0
        run 3.0

        0
