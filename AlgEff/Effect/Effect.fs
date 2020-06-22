namespace AlgEff.Effect

/// An individual effectful operation, such as writing to a console.
/// The type parameter is used to chain effects together.
[<AbstractClass>]
type Effect<'a>() =

    /// Maps a function over this effect.
    abstract member Map : ('a -> 'b) -> Effect<'b>

/// A chain of effects (a.k.a. "free monad") that requires a particular
/// context type ('ctx) and returns a particular result type ('res).
type EffectChain<'ctx, 'res> =

    /// One link in a chain of effects.
    | Free of Effect<EffectChain<'ctx, 'res>>

    /// Last link in a chain of effects.
    | Pure of 'res

module EffectChain =

    /// Binds two effect chains together in the same context.
    let rec bind (f : _ -> EffectChain<'ctx, _>) (chain : EffectChain<'ctx, _>) =
        match chain with
            | Free effect ->
                effect.Map(bind f) |> Free
            | Pure x -> f x

/// Effect chain builder.
type EffectChainBuilder() =
    member __.Bind(chain, f) = EffectChain.bind f chain
    member __.Return(value) = Pure value
    member __.ReturnFrom(value) = value
    member __.Zero() = Pure ()

[<AutoOpen>]
module AutoOpen =

    /// Effect chain builder.
    let effect = EffectChainBuilder()
