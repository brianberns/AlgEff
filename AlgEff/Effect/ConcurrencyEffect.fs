namespace AlgEff.Effect

/// Base concurrency effect type.
/// See https://kcsrk.info/ocaml/multicore/2015/05/20/effects-multicore/
[<AbstractClass>]
type ConcurrencyEffect<'ctx, 'next>() =
    inherit Effect<'next>()

    /// Type-safe subtype enumeration.
    abstract member Case : ConcurrencyEffectSum<'ctx, 'next>

/// 
and ForkEffect<'ctx, 'next>(program : Program<'ctx, unit>, cont : unit -> 'next) =
    inherit ConcurrencyEffect<'ctx, 'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        ForkEffect(program, cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Fork this

    /// Program to fork.
    member __.Program = program

    /// Continuation to next effect.
    member __.Cont = cont

/// 
and YieldEffect<'ctx, 'next>(cont : unit -> 'next) =
    inherit ConcurrencyEffect<'ctx, 'next>()

    /// Maps a function over this effect.
    override __.Map(f) =
        YieldEffect<'ctx, _>(cont >> f) :> _

    /// Type-safe subtype enumeration.
    override this.Case = Yield this

    /// Continuation to next effect.
    member __.Cont = cont

/// Sum type for concurrency effects.
and ConcurrencyEffectSum<'ctx, 'next> =
    | Fork of ForkEffect<'ctx, 'next>
    | Yield of YieldEffect<'ctx, 'next>

/// Concurrency context requirement.
type ConcurrencyContext = interface end

module Concurrency =

    /// 
    let fork<'ctx when 'ctx :> ConcurrencyContext> (program : Program<'ctx, unit>) : Program<'ctx, _> =
        Free (ForkEffect(program, Pure))

    /// 
    let yld<'ctx when 'ctx :> ConcurrencyContext> : Program<'ctx, _> =
        Free (YieldEffect<'ctx, _>(Pure))
