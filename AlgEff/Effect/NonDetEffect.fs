namespace AlgEff.Effect

/// Base non-deterministic effect type.
/// See https://www.eff-lang.org/handlers-tutorial.pdf, section 2.3.
[<AbstractClass>]
type NonDetEffect<'next>() =
    inherit Effect<'next>()

    /// Type-safe subtype enumeration.
    abstract member Case : NonDetEffectSum<'next>

/// Generates a non-deterministic boolean.
and DecideEffect<'next>(cont : bool -> 'next) =
    inherit NonDetEffect<'next>()

    /// Maps a function over this effect.
    override _.Map(f) =
        DecideEffect(cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Decide this

    /// Continuation to next effect.
    member _.Cont = cont

/// Triggers backtracking.
and FailEffect<'next>(cont : unit -> 'next) =
    inherit NonDetEffect<'next>()

    /// Maps a function over this effect.
    override _.Map(f) =
        FailEffect(cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Fail this

    /// Continuation to next effect.
    member _.Cont = cont

/// Sum type for non-deterministic effects.
and NonDetEffectSum<'next> =
    | Decide of DecideEffect<'next>
    | Fail of FailEffect<'next>

/// Non-deterministic context requirement.
type NonDetContext = interface end

module NonDet =

    /// Generates a non-deterministic boolean.
    let decide<'ctx when 'ctx :> NonDetContext> : Program<'ctx, _> =
        Free (DecideEffect(Pure))

    /// Chooses between two values non-deterministically.
    let choose x y =
        effect {
            let! flag = decide
            return
                if flag then x else y
        }

    /// Triggers backtracking.
    let fail<'ctx when 'ctx :> NonDetContext> : Program<'ctx, _> =
        Free (FailEffect(Pure))
