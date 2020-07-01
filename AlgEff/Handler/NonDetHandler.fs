namespace AlgEff.Handler

open AlgEff.Effect

/// Always picks the true choice.
type PickTrue<'env, 'ret when 'env :> NonDetContext and 'env :> Environment<'ret>>(env : 'env) =
    inherit SimpleHandler<'env, 'ret, Unit>()

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
type PickMax<'env, 'ret when 'env :> NonDetContext and 'env :> Environment<'ret> and 'ret : comparison>(env : 'env) =
    inherit SimpleHandler<'env, 'ret, Unit>()

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
type PickAll<'env, 'ret when 'env :> NonDetContext and 'env :> Environment<'ret>>(env : 'env) =
    inherit SimpleHandler<'env, 'ret, Unit>()

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
