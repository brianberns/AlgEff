namespace AlgEff

open AlgEff.Effect
open AlgEff.Handler

type ProgramEnv<'ret>() as this =
    inherit Environment<'ret>()

    let handler =
        Handler.combine2
            (ActualConsoleHandler(this))
            (PureLogHandler(this))
    
    interface ConsoleContext
    interface LogContext

    member _.Handler = handler

module Program =

    let program =
        effect {
            do! Console.writeln "What is your name?"
            let! name = Console.readln
            do! Console.writelnf "Hello %s" name
            do! Log.writef "Name is %s" name
            return name
        }

    [<EntryPoint>]
    let main argv =
        let name, (Unit, log) =
            ProgramEnv().Handler.Run(program)
        printfn "Log: %A" log
        printfn "Name: %s" name
        0
