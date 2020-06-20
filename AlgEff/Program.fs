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

    type ProgramHandler<'res>() as this =

        let consoleHandler = PureConsoleHandler.Create<_, 'res>(this, ["John"])
        let logHandler = PureLogHandler.Create<_, 'res>(this)

        interface ConsoleContext
        interface LogContext

        member __.ConsoleHandler = consoleHandler :> EffectHandler<_, _, _>
        member __.LogHandler = logHandler :> EffectHandler<_, _, _>

    module ProgramHandler =

        let create (_ : EffectChain<ProgramHandler<'res>, 'res>) =
            ProgramHandler<'res>()

        let run (_ : EffectChain<ProgramHandler<'res>, 'res>) =
            let handler = ProgramHandler<'res>()
            let state = handler.ConsoleHandler.Start, handler.LogHandler.Start
            state

    [<EntryPoint>]
    let main argv =
        let program = greet () |> ProgramHandler.run
        printfn "%A" program
        0
