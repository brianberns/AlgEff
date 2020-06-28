namespace AlgEff.Handler

open AlgEff.Effect

type Dummy = Dummy   // why doesn't unit work?

type PickTrue<'ctx, 'res when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'res>>(context : 'ctx) =
    inherit EffectHandler<'ctx, 'res, Dummy, Dummy>()

    override __.Start = Dummy

    override __.TryStep(_, effect, cont) =
        match effect with
            | :? NonDetEffect<EffectChain<'ctx, 'res>> as nonDetEff ->
                match nonDetEff.Case with
                    | Decide eff ->
                        let next = eff.Cont(true)
                        cont Dummy next
                    |> Some
            | _ -> None

    override __.Finish(dummy) = dummy

    (*
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
