namespace AlgEff.Handler

open AlgEff.Effect

/// Pure state handler.
type PureStateHandler<'state, 'ctx, 'res when 'ctx :> StateContext<'state> and 'ctx :> ConcreteContext<'res>>(initial : 'state, context : 'ctx) =
    inherit EffectHandler<'ctx, 'res, 'state, 'state>()

    override __.Start = initial

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

        this.Adapt<_, 'outState> step state effect cont

    override __.Finish(state) = state
