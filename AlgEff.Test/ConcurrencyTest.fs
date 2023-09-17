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

    member _.Handler = handler

[<TestClass>]
type ConcurrencyTest() =

    [<TestMethod>]
    member _.Concurrency() =

        let rec program id depth =
            effect {
                do! Log.writef "Starting %d" id
                if depth > 0 then
                    let depth' = depth - 1
                    let idLeft = id * 2 + 1
                    do! Log.writef "Forking %d" idLeft
                    do! Concurrency.fork <| program idLeft depth'
                    let idRight = id * 2 + 2
                    do! Log.writef "Forking %d" idRight
                    do! Concurrency.fork <| program idRight depth'
                else
                    do! Log.writef "Yielding in %d" id
                    do! Concurrency.yld
                    do! Log.writef "Resumed %d" id
                do! Log.writef "Finishing %d" id
                do! Concurrency.exit
            }

        let (), (queue, log) =
            PureConcurrencyLogEnv().Handler.Run(program 0 2)
        Assert.IsTrue(Queue.isEmpty queue)
        Assert.AreEqual(
            [
                "Starting 0"
                "Forking 1"
                "Starting 1"
                "Forking 3"
                "Starting 3"
                "Yielding in 3"
                "Forking 2"
                "Starting 2"
                "Forking 5"
                "Starting 5"
                "Yielding in 5"
                "Forking 4"
                "Starting 4"
                "Yielding in 4"
                "Resumed 3"
                "Finishing 3"
                "Finishing 0"
                "Forking 6"
                "Starting 6"
                "Yielding in 6"
                "Resumed 5"
                "Finishing 5"
                "Finishing 1"
                "Resumed 4"
                "Finishing 4"
                "Finishing 2"
                "Resumed 6"
                "Finishing 6"
            ], log)
