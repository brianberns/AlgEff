namespace AlgEff.Handler

open AlgEff.Effect

/// https://stackoverflow.com/questions/33464319/implement-a-queue-type-in-f
type Queue<'a> = 'a list * 'a list

module Queue =

    let empty =
        [], []

    let isEmpty = function
        | ([], []) -> true
        | _ -> false

    let enqueue e (fs, bs) =
        e :: fs, bs

    let dequeue = function
        | ([], []) -> failwith "Empty queue"
        | (fs, b :: bs) -> b, (fs, bs)
        | (fs, []) ->
            let bs = List.rev fs
            bs.Head, ([], bs.Tail)

/// Pure concurrency handler.
type PureConcurrencyHandler<'env when 'env :> ConcurrencyContext and 'env :> Environment<unit>>(env : 'env) =
    inherit SimpleHandler<'env, unit, Queue<unit -> Program<'env, unit>>>()

    /// Starts with an empty queue of programs.
    override _.Start = Queue.empty

    /// Manages program control.
    override _.TryStep<'stx>(queue, effect, cont : HandlerCont<_, _, _, 'stx>) =

        /// Runs the next queued program.
        let run queue =
            let getProgram, queue' = queue |> Queue.dequeue
            cont queue' <| getProgram ()

        Handler.tryStep effect (fun (concurrencyEff : ConcurrencyEffect<'env, _>) ->
            match concurrencyEff.Case with
                | Fork eff ->
                    let queue' = queue |> Queue.enqueue eff.Cont
                    cont queue' eff.Program
                | Yield eff ->
                    queue |> Queue.enqueue eff.Cont |> run
                | Exit eff ->
                    if queue |> Queue.isEmpty then
                        cont queue <| eff.Cont ()
                    else
                        run queue)
