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

    type ProgramHandler<'res>(consoleInput) as this =

        let consoleHandler = ConsoleHandler.createPureCtx<_, 'res>(this, consoleInput)
        let logHandler = LogHandler.createPureCtx<_, 'res>(this)
        let handler = EffectHandler.combine consoleHandler logHandler

        interface ConsoleContext
        interface LogContext

        member __.Run(program) =
            handler |> EffectHandler.run program

    module ProgramHandler =

        let run input program =
            ProgramHandler(input).Run(program)

    [<EntryPoint>]
    let main argv =
        let name, (console, log) = greet () |> ProgramHandler.run ["John"]
        printfn "Console input: %A" console.Input
        printfn "Console output: %A" console.Output
        printfn "Log: %A" log
        printfn "Name: %s" name
        0
