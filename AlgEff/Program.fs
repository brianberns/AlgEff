namespace AlgEff

module Program =

    [<EntryPoint>]
    let main argv =

        let logSqrt x =
            log {
                do! LogEffect.logf "x: %A" x
                let result = sqrt x
                do! LogEffect.logf "result: %A" result
                return result
            }

        let run x =

            let result, log =
                logSqrt x
                    |> LogEffect.pureHandler

            printfn ""
            printfn "Log: %A messages" log.Length
            for msg in log |> List.rev do
                printfn "%s" msg
            printfn "Final result: %A" result

        run 2.0
        run 3.0

        0
