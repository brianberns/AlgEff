namespace AlgEff

module Program =

    let greet () =
        effect {
            do! Console.writeln "What is your name?"
            let! name = Console.readln
            do! Console.writelnf "Hello %s" name
            do! Log.writef "Name is %s" name
            return name
        }

    type ProgramHandler<'res>() as this =

        let consoleHandler = PureConsoleHandler.Create<_, 'res>(this, ["John"])
        let logHandler = PureLogHandler.Create<_, 'res>(this)
        let handler = CombinedEffectHandler(consoleHandler, logHandler)

        interface ConsoleContext
        interface LogContext

        member private __.Handler = handler

        static member Run(program) =
            ProgramHandler<'res>().Handler
                |> EffectHandler.run program

    [<EntryPoint>]
    let main argv =
        let name, (console, log) = greet () |> ProgramHandler.Run
        printfn "Console input: %A" console.Input
        printfn "Console output: %A" console.Output
        printfn "Log: %A" log
        printfn "Name: %s" name
        0
