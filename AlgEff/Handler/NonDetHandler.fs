namespace AlgEff.Handler

open AlgEff.Effect

type Dummy = Dummy   // unit doesn't work

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

    override __.Finish(Dummy) = Dummy

type PickMax<'ctx, 'res when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'res> and 'res : comparison>(context : 'ctx) =
    inherit EffectHandler<'ctx, 'res, Dummy, Dummy>()

    override __.Start = Dummy

    override __.TryStep(_, effect, cont) =
        match effect with
            | :? NonDetEffect<EffectChain<'ctx, 'res>> as nonDetEff ->
                match nonDetEff.Case with
                    | Decide eff ->
                        let outStateT, resT = eff.Cont(true) |> cont Dummy
                        let outStateF, resF = eff.Cont(false) |> cont Dummy
                        if resT > resF then
                            outStateT, resT
                        else
                            outStateF, resF
                    |> Some
            | _ -> None

    override __.Finish(Dummy) = Dummy
