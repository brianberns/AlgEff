namespace AlgEff

open AlgEff.Effect
open AlgEff.Handler

type ProgramContext<'ret>() as this =
    inherit ConcreteContext<'ret>()

    let handler =
        Handler.combine2
            (ActualConsoleHandler(this))
            (PureLogHandler(this))
    
    interface ConsoleContext
    interface LogContext

    member __.Handler = handler

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
        let name, (console, log) =
            ProgramContext().Handler.Run(program)
        printfn "Console: %A" console
        printfn "Log: %A" log
        printfn "Name: %s" name
        0
