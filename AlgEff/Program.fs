namespace AlgEff

type ProgramContext<'res>(consoleInput) as this =

    let consoleHandler = ConsoleHandler.createPure<_, 'res>(this, consoleInput)
    let logHandler = LogHandler.createPure<_, 'res>(this)
    let handler = EffectHandler.combine consoleHandler logHandler
    
    interface ConsoleContext
    interface LogContext

    member __.Run(program) =
        handler |> EffectHandler.run program

module Program =

    let greet () =
        effect {
            do! Console.writeln "What is your name?"
            let! name = Console.readln
            do! Console.writelnf "Hello %s" name
            do! Log.writef "Name is %s" name
            return name
        }

    [<EntryPoint>]
    let main argv =
        let name, (console, log) = greet () |> ProgramContext(["John"]).Run
        printfn "Console input: %A" console.Input
        printfn "Console output: %A" console.Output
        printfn "Log: %A" log
        printfn "Name: %s" name
        0
