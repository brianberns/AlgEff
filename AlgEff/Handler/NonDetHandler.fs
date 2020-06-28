namespace AlgEff.Handler

open AlgEff.Effect

module NonDetHandler =

    do ()

    (*
    let pickTrue<'ctx, 'res when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'res>>
        (_ : 'ctx) =

        let step ((), (nonDetEff : NonDetEffect<EffectChain<'ctx, 'res>>)) =
            match nonDetEff.Case with
                | Decide eff ->
                    let next = eff.Cont(true)
                    (), next

        EffectHandler.adapt () step id

    let pickMax<'ctx, 'res when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'res> and 'res : comparison>
        (_ : 'ctx) =

        let step ((), (nonDetEff : NonDetEffect<EffectChain<'ctx, 'res>>)) =
            match nonDetEff.Case with
                | Decide eff ->
                    let next =
                        effect {
                            let! xt = eff.Cont(true)
                            let! xf = eff.Cont(false)
                            return max xt xf
                        }
                    (), next

        EffectHandler.adapt () step id
    *)
