namespace AlgEff.Effect

/// Base non-deterministic effect type.
[<AbstractClass>]
type NonDetEffect<'next>() =
    inherit Effect<'next>()

    /// Type-safe subtype enumeration.
    abstract member Case : NonDetEffectSum<'next>

and DecideEffect<'next>(cont : bool -> 'next) =
    inherit NonDetEffect<'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        DecideEffect(cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Decide this

    /// Continuation to next effect.
    member __.Cont = cont

/// Sum type for non-deterministic effects.
and NonDetEffectSum<'next> =
    | Decide of DecideEffect<'next>

/// Non-deterministic context requirement.
type NonDetContext = interface end

module NonDet =

    let decide<'ctx when 'ctx :> NonDetContext> : Program<'ctx, _> =
        Free (DecideEffect(Pure))

    let choose x y =
        effect {
            let! flag = decide
            return
                if flag then x else y
        }
