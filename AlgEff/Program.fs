namespace AlgEff

module Program =

    let greet () =
        effect {
            do! Console.writeln "What is your name?"
            let! name = Console.readln
            do! Console.writelnf "Hello %s" name
            do! Log.writef "Name is %s" name
        }

    let dump log =
        printfn ""
        printfn "Log contains %A entries:" (log |> List.length)
        for msg in log do
            printfn "   %s" msg

    type Handler() =
        interface ConsoleHandler
        interface LogHandler

        member __.Run<'res>() =
            let consoleHandler = ConsoleHandler.handle<OpChain<Handler, 'res>> [ "John" ]
            let logHandler = LogHandler.handle<OpChain<Handler, 'res>>
            let state = consoleHandler.Init, logHandler.Init
            ()

    [<EntryPoint>]
    let main argv =
        let program : OpChain<Handler, _> = greet ()
        printfn "%A" program
        0
