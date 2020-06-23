namespace AlgEff.Handler

open AlgEff.Effect

module StateHandler =

    /// Pure state handler.
    let createPure<'state, 'ctx, 'res when 'ctx :> StateContext<'state> and 'ctx :> ConcreteContext<'res>>
        (initial : 'state) (_ : 'ctx) =

        let start = initial

        let step (state, (stateEff : StateEffect<'state, EffectChain<'ctx, 'res>>)) =
            match stateEff.Case with
                | Put eff ->
                    let state' = eff.Value
                    let next = eff.Cont()
                    state', next
                | Get eff ->
                    let next = eff.Cont(state)
                    state, next

        EffectHandler.adapt start step id
