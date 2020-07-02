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
                do! Log.writef "Starting %d" id
                if depth > 0 then
                    let depth' = depth - 1

                        // fork left child
                    let idLeft = id * 2 + 1
                    do! Log.writef "Forking %d" idLeft
                    do! Concurrency.fork <| program idLeft depth'

                        // fork right child
                    let idRight = id * 2 + 2
                    do! Log.writef "Forking %d" idRight
                    do! Concurrency.fork <| program idRight depth'

                else
                    do! Log.writef "Yielding in %d" id
                    do! Concurrency.yld
                    do! Log.writef "Resumed %d" id
                do! Log.writef "Finishing %d" id
            }

        let (), (queue, log) =
            PureConcurrencyLogEnv().Handler.Run(program 0 2)
        // Assert.IsTrue(Queue.isEmpty queue)
        for str in log do
            printfn "%A" str
