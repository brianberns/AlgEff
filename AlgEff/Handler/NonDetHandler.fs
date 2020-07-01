namespace AlgEff.Handler

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
                | Fail _ -> []

        this.Adapt<_, 'stx> step Unit effect cont

    override __.Finish(Unit) = Unit

type PickMax<'ctx, 'ret when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'ret> and 'ret : comparison>(context : 'ctx) =
    inherit Handler<'ctx, 'ret, Unit, Unit>()

    override __.Start = Unit

    override this.TryStep(_, effect, cont) =

        let step Unit (nonDetEff : NonDetEffect<_>) cont =
            match nonDetEff.Case with
                | Decide eff ->
                    let stxT, resT = eff.Cont(true) |> cont Unit |> List.exactlyOne
                    let stxF, resF = eff.Cont(false) |> cont Unit |> List.exactlyOne
                    if resT > resF then
                        [ stxT, resT ]
                    else
                        [ stxF, resF ]
                | Fail _ -> []

        this.Adapt<_, 'stx> step Unit effect cont

    override __.Finish(Unit) = Unit

type PickAll<'ctx, 'ret when 'ctx :> NonDetContext and 'ctx :> ConcreteContext<'ret>>(context : 'ctx) =
    inherit Handler<'ctx, 'ret, Unit, Unit>()

    override __.Start = Unit

    override this.TryStep<'stx>(_, effect, cont) =

        let step Unit (nonDetEff : NonDetEffect<_>) cont =
            match nonDetEff.Case with
                | Decide eff ->
                    let pairsT = eff.Cont(true) |> cont Unit
                    let pairsF = eff.Cont(false) |> cont Unit
                    List.append pairsT pairsF
                | Fail _ -> []

        this.Adapt<_, 'stx> step Unit effect cont

    override __.Finish(Unit) = Unit
