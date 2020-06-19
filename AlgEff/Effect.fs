namespace AlgEff

/// An individual effectful operation, such as writing to a console.
/// The type parameter is used to chain effects together.
type Effect<'a> =

    /// Maps a function over an effect.
    abstract member Map : ('a -> 'b) -> Effect<'b>

/// A chain of effects (a.k.a. "free monad").
type EffectChain<'ctx, 'res> =

    /// One link in a chain of effects.
    | Free of Effect<EffectChain<'ctx, 'res>>

    /// Last link in a chain of effects.
    | Pure of 'res

module EffectChain =

    /// Binds two effect chains together using the same context.
    let rec bind (f : _ -> EffectChain<'ctx, _>) (chain : EffectChain<'ctx, _>) =
        match chain with
            | Free effect ->
                effect.Map(bind f) |> Free
            | Pure x -> f x

type EffectChainBuilder() =
    member __.Bind(chain, f) = EffectChain.bind f chain
    member __.Return(x) = Pure x
    member __.ReturnFrom(x) = x
    member __.Zero() = Pure ()

[<AutoOpen>]
module AutoOpen =

    let effect = EffectChainBuilder()
