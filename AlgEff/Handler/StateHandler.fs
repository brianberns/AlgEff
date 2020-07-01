namespace AlgEff.Handler

open AlgEff.Effect

/// Pure state handler.
type PureStateHandler<'state, 'env, 'ret when 'env :> StateContext<'state> and 'env :> Environment<'ret>>(initial : 'state, env : 'env) =
    inherit SimpleHandler<'env, 'ret, 'state>()

    /// Start with given initial state.
    override __.Start = initial

    /// Sets or gets the state.
    override this.TryStep(state, effect, cont) =

        let step state (stateEff : StateEffect<_, _>) cont =
            match stateEff.Case with
                | Put eff ->
                    let state' = eff.Value
                    let next = eff.Cont()
                    cont state' next
                | Get eff ->
                    let next = eff.Cont(state)
                    cont state next

        this.Adapt<_, 'stx> step state effect cont
