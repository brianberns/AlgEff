namespace AlgEff.Test

open Microsoft.VisualStudio.TestTools.UnitTesting

open AlgEff.Effect
open AlgEff.Handler

type PureConcurrencyLogEnv() as this =
    inherit Environment<unit>()

    let handler =
        Handler.combine2
            (PureConcurrencyHandler(this))
            (PureLogHandler<_, unit>(this))
    
    interface ConcurrencyContext
    interface LogContext

    member __.Handler = handler

[<TestClass>]
type ConcurrencyTest() =

    [<TestMethod>]
    member __.Concurrency() =

        let rec program id depth =
            effect {
                do! Log.writef "Starting number %d!" id
                if depth > 0 then 
                    do! Log.writef "Forking number %d!" (id * 2 + 1)
                    do! Concurrency.fork <| program (id * 2 + 1) (depth - 1)
                    do! Log.writef "Forking number %d!" (id * 2 + 2)
                    do! Concurrency.fork <| program (id * 2 + 2) (depth - 1)
                else 
                    do! Log.writef "Yielding in number %d!" id
                    do! Concurrency.yld
                    do! Log.writef "Resumed number %d!" id
                do! Log.writef "Finishing number %d!" id
            }

        let (), (queue, log) =
            PureConcurrencyLogEnv().Handler.Run(program 0 2)
        printfn "%A" queue
        printfn "%A" log
