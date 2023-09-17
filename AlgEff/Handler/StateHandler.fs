namespace AlgEff.Handler

open AlgEff.Effect

/// Pure state handler.
type PureStateHandler<'state, 'env, 'ret when 'env :> StateContext<'state> and 'env :> Environment<'ret>>(initial : 'state, env : 'env) =
    inherit SimpleHandler<'env, 'ret, 'state>()

    /// Start with given initial state.
    override _.Start = initial

    /// Sets or gets the state.
    override _.TryStep(state, effect, cont : HandlerCont<_, _, _, 'stx>) =
        Handler.tryStep effect (fun (stateEff : StateEffect<_, _>) ->
            match stateEff.Case with
                | Put eff ->
                    let state' = eff.Value
                    let next = eff.Cont()
                    cont state' next
                | Get eff ->
                    let next = eff.Cont(state)
                    cont state next)
