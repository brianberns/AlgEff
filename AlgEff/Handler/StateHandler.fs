namespace AlgEff.Handler

open AlgEff.Effect

/// Pure state handler.
type PureStateHandler<'state, 'ctx, 'res when 'ctx :> StateContext<'state> and 'ctx :> ConcreteContext<'res>>(initial : 'state, context : 'ctx) =
    inherit EffectHandler<'ctx, 'res, 'state, 'state>()

    override __.Start = initial

    override __.TryStep(state, effect, cont) =
        match effect with
            | :? StateEffect<'state, EffectChain<'ctx, 'res>> as stateEff ->
                match stateEff.Case with
                    | Put eff ->
                        let state' = eff.Value
                        let next = eff.Cont()
                        cont state' next
                    | Get eff ->
                        let next = eff.Cont(state)
                        cont state next
                    |> Some
            | _ -> None

    override __.Finish(state) = state
