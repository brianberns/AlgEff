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

        member __.Handler = handler :> EffectHandler<_, _, _>

    module ProgramHandler =

        let run (program : EffectChain<ProgramHandler<'res>, 'res>) =

            let handler = ProgramHandler<'res>().Handler

            let rec loop state = function
                | Free effect ->
                    let choice =
                        match effect with
                            | :? ConsoleEff<EffectChain<ProgramHandler<'res>, 'res>> as consoleEff -> Choice1Of2 consoleEff
                            | :? LogEff<EffectChain<ProgramHandler<'res>, 'res>> as logEff -> Choice2Of2 logEff
                            | _ -> failwith "Unhandled effect"
                    let state', next = handler.Step(state, choice)
                    loop state' next
                | Pure result ->
                    state, result

            loop handler.Start program
                

    [<EntryPoint>]
    let main argv =
        let (console, log), name = greet () |> ProgramHandler.run
        printfn "Console: %A" console
        printfn "Log: %A" log
        printfn "Name: %s" name
        0
