namespace AlgEff

module Program =

    let greet () =
        effect {
            do! Console.writeln "What is your name?"
            let! name = Console.readln
            do! Console.writelnf "Hello %s" name
            // do! Log.writef "Name is %s" name
            return 0
        }

    let dump log =
        printfn ""
        printfn "Log contains %A entries:" (log |> List.length)
        for msg in log do
            printfn "   %s" msg

    type Handler<'res>() =

        let consoleHandler = ConsoleHandler<EffectChain<Handler<'res>, 'res>>(["John"])
        // let logHandler = LogHandler.handle<OpChain<Handler<'res>, 'res>>

        let consoleHandlerCarton = ConsoleHandlerCartonImpl.Create(consoleHandler)

        interface ConsoleContext with
            member __.ApplyOp(op) = consoleHandlerCarton.ApplyOp(op)

    [<EntryPoint>]
    let main argv =
        let program : EffectChain<Handler<int>, int> = greet ()
        printfn "%A" program
        0
