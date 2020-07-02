namespace AlgEff.Handler

open AlgEff.Effect

/// https://stackoverflow.com/questions/33464319/implement-a-queue-type-in-f
type Queue<'a> =
    | Queue of 'a list * 'a list

module Queue =

    let empty = Queue ([], [])

    let enqueue e = function
        | Queue (fs, bs) -> Queue (e :: fs, bs)

    let dequeue = function
        | Queue ([], []) -> failwith "Empty queue"
        | Queue (fs, b :: bs) -> b, Queue (fs, bs)
        | Queue (fs, []) -> 
            let bs = List.rev fs
            bs.Head, Queue ([], bs.Tail)

/// Pure concurrency handler.
type PureConcurrencyHandler<'env when 'env :> ConcurrencyContext and 'env :> Environment<unit>>(env : 'env) =
    inherit SimpleHandler<'env, unit, Queue<unit -> Program<'env, unit>>>()

    /// 
    override __.Start = Queue.empty

    /// 
    override __.TryStep<'stx>(queue, effect, cont : HandlerCont<_, _, _, 'stx>) =
        Handler.adapt effect (fun (concurrencyEff : ConcurrencyEffect<'env, _>) ->
            match concurrencyEff.Case with
                | Fork eff ->
                    let queue' = queue |> Queue.enqueue eff.Cont
                    cont queue' eff.Program
                | Yield eff ->
                    let queue' = queue |> Queue.enqueue eff.Cont
                    let getProgram, queue'' = queue' |> Queue.dequeue
                    cont queue'' <| getProgram ())
