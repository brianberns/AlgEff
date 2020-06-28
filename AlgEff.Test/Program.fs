namespace AlgEff

open AlgEff.Effect
open AlgEff.Handler

type ProgramContext<'res>() as this =
    inherit ConcreteContext<'res>()

    let handler =
        let consoleHandler = PureConsoleHandler(["John"], this)
        let logHandler = PureLogHandler(this)
        CombinedEffectHandler(consoleHandler, logHandler)
    
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
