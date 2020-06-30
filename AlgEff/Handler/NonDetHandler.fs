﻿namespace AlgEff.Handler

open AlgEff.Effect

type PickTrue<'ctx, 'ret when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'ret>>(context : 'ctx) =
    inherit Handler<'ctx, 'ret, Unit, Unit>()

    override __.Start = Unit

    override this.TryStep<'stx>(Unit, effect, cont) =

        let step Unit (nonDetEff : NonDetEffect<_>) cont =
            match nonDetEff.Case with
                | Decide eff ->
                    let next = eff.Cont(true)
                    cont Unit next

        this.Adapt<_, 'stx> step Unit effect cont

    override __.Finish(Unit) = Unit

type PickMax<'ctx, 'ret when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'ret> and 'ret : comparison>(context : 'ctx) =
    inherit Handler<'ctx, 'ret, Unit, Unit>()

    override __.Start = Unit

    override this.TryStep(_, effect, cont) =

        let step Unit (nonDetEff : NonDetEffect<_>) cont =
            match nonDetEff.Case with
                | Decide eff ->
                    let outStateT, resT = eff.Cont(true) |> cont Unit
                    let outStateF, resF = eff.Cont(false) |> cont Unit
                    if resT > resF then
                        outStateT, resT
                    else
                        outStateF, resF

        this.Adapt<_, 'stx> step Unit effect cont

    override __.Finish(Unit) = Unit
