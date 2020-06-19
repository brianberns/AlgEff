namespace AlgEff

module Program =

    let greet () =
        effect {
            do! Console.writeln "What is your name?"
            let! name = Console.readln
            do! Console.writelnf "Hello %s" name
            do! Log.writef "Name is %s" name
            return 0
        }

    type ProgramHandler<'res>() =

        let consoleHandler = PureConsoleHandler<ProgramHandler<'res>, 'res>(["John"])
        let consoleHandlerCarton = ConsoleHandlerCartonImpl.Create(consoleHandler)

        let logHandler = PureLogHandler<ProgramHandler<'res>, 'res>()
        let logHandlerCarton = LogHandlerCartonImpl.Create(logHandler)

        interface ConsoleHandlerCarton with
            member __.ApplyOp(op) = consoleHandlerCarton.ApplyOp(op)

        interface LogHandlerCarton with
            member __.ApplyOp(op) = logHandlerCarton.ApplyOp(op)

        static member Create(_ : EffectChain<ProgramHandler<'res>, 'res>) =
            ProgramHandler<'res>()

    [<EntryPoint>]
    let main argv =
        let program = greet () |> ProgramHandler.Create
        printfn "%A" program
        0
