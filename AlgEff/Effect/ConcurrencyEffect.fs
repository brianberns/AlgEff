namespace AlgEff.Effect

/// Base concurrency effect type.
/// See https://kcsrk.info/ocaml/multicore/2015/05/20/effects-multicore/
[<AbstractClass>]
type ConcurrencyEffect<'ctx, 'next>() =
    inherit Effect<'next>()

    /// Type-safe subtype enumeration.
    abstract member Case : ConcurrencyEffectSum<'ctx, 'next>

/// Forks the given program.
and ForkEffect<'ctx, 'next>(program : Program<'ctx, unit>, cont : unit -> 'next) =
    inherit ConcurrencyEffect<'ctx, 'next>()

    /// Maps a function over this effect.
    override _.Map(f) =
        ForkEffect(program, cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Fork this

    /// Program to fork.
    member _.Program = program

    /// Continuation to next effect.
    member _.Cont = cont

/// Yields control from the current program.
and YieldEffect<'ctx, 'next>(cont : unit -> 'next) =
    inherit ConcurrencyEffect<'ctx, 'next>()

    /// Maps a function over this effect.
    override _.Map(f) =
        YieldEffect<'ctx, _>(cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Yield this

    /// Continuation to next effect.
    member _.Cont = cont

/// Exits the current program.
and ExitEffect<'ctx, 'next>(cont : unit -> 'next) =
    inherit ConcurrencyEffect<'ctx, 'next>()

    /// Maps a function over this effect.
    override _.Map(f) =
        ExitEffect<'ctx, _>(cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Exit this
 
    /// Continuation to next effect.
    member _.Cont = cont

/// Sum type for concurrency effects.
and ConcurrencyEffectSum<'ctx, 'next> =
    | Fork of ForkEffect<'ctx, 'next>
    | Yield of YieldEffect<'ctx, 'next>
    | Exit of ExitEffect<'ctx, 'next>

/// Concurrency context requirement.
type ConcurrencyContext = interface end

module Concurrency =

    /// Forks the given program.
    let fork<'ctx when 'ctx :> ConcurrencyContext> (program : Program<'ctx, unit>) : Program<'ctx, _> =
        Free (ForkEffect(program, Pure))

    /// Yields control from the current program.
    let yld<'ctx when 'ctx :> ConcurrencyContext> : Program<'ctx, _> =
        Free (YieldEffect<'ctx, _>(Pure))

    /// Exits the current program.
    let exit<'ctx when 'ctx :> ConcurrencyContext> : Program<'ctx, _> =
        Free (ExitEffect<'ctx, _>(Pure))
