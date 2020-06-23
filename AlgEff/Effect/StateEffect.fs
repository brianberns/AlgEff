namespace AlgEff.Effect

/// Base state effect type.
[<AbstractClass>]
type StateEffect<'state, 'next>() =
    inherit Effect<'next>()

    /// Type-safe subtype enumeration.
    abstract member Case : StateEffectSum<'state, 'next>

/// Sets the current state.
and PutEffect<'state, 'next>(value : 'state, cont : unit -> 'next) =
    inherit StateEffect<'state, 'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        PutEffect(value, cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Put this

    /// Value to set.
    member __.Value = value

    /// Continuation to next effect.
    member __.Cont = cont

/// Gets the current state.
and GetEffect<'state, 'next>(cont : 'state -> 'next) =
    inherit StateEffect<'state, 'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        GetEffect(cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Get this

    /// Continuation to next effect.
    member __.Cont = cont

/// Sum type for state effects.
and StateEffectSum<'state, 'next> =
    | Put of PutEffect<'state, 'next>
    | Get of GetEffect<'state, 'next>

/// State context requirement.
type StateContext<'state> = interface end

module State =

    /// Sets the current state
    let put<'state, 'ctx when 'ctx :> StateContext<'state>> (value : 'state) : EffectChain<'ctx, _> =
        Free (PutEffect(value, Pure))

    /// Gets the current state
    let get<'state, 'ctx when 'ctx :> StateContext<'state>> : EffectChain<'ctx, 'state> =
        Free (GetEffect(Pure))
