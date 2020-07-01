namespace AlgEff.Handler

open AlgEff.Effect

/// Always picks the true choice.
type PickTrue<'ctx, 'ret when 'ctx :> NonDetContext and 'ctx :> ContextSatisfier<'ret>>(context : 'ctx) =
    inherit SimpleHandler<'ctx, 'ret, Unit>()

    override __.Start = Unit

    override this.TryStep<'stx>(Unit, effect, cont) =

        let step Unit (nonDetEff : NonDetEffect<_>) cont =
            match nonDetEff.Case with
                | Decide eff ->
                    let next = eff.Cont(true)
                    cont Unit next
                | Fail _ -> []

        this.Adapt<_, 'stx> step Unit effect cont

/// Picks the choice with the maximum value.
type PickMax<'ctx, 'ret when 'ctx :> NonDetContext and 'ctx :> ContextSatisfier<'ret> and 'ret : comparison>(context : 'ctx) =
    inherit SimpleHandler<'ctx, 'ret, Unit>()

    override __.Start = Unit

    override this.TryStep(_, effect, cont) =

        let step Unit (nonDetEff : NonDetEffect<_>) cont =
            match nonDetEff.Case with
                | Decide eff ->
                    let resT, stxT = eff.Cont(true) |> cont Unit |> List.exactlyOne
                    let resF, stxF = eff.Cont(false) |> cont Unit |> List.exactlyOne
                    if resT > resF then [ resT, stxT ]
                    else [ resF, stxF ]
                | Fail _ -> []

        this.Adapt<_, 'stx> step Unit effect cont

/// Picks all the choices.
type PickAll<'ctx, 'ret when 'ctx :> NonDetContext and 'ctx :> ContextSatisfier<'ret>>(context : 'ctx) =
    inherit SimpleHandler<'ctx, 'ret, Unit>()

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
