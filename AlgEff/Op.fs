namespace AlgEff

/// An individual effectful operation, such as writing to a console.
/// The type parameter is used to chain operations together.
type Op<'a> =

    /// Maps a function over an operation.
    abstract member Map : ('a -> 'b) -> Op<'b>

/// A chain of operations (a.k.a. "free monad").
type OpChain<'ctx, 'res> =

    /// One link in a chain of effects.
    | Free of Op<OpChain<'ctx, 'res>>

    /// Last link in a chain of effects.
    | Pure of 'res

module OpChain =

    /// Binds two operation chains together.
    let rec bind f = function
        | Free effect ->
            effect.Map(bind f) |> Free
        | Pure x -> f x

type OpChainBuilder() =
    member __.Bind(chain, f) = OpChain.bind f chain
    member __.Return(x) = Pure x
    member __.ReturnFrom(x) = x
    member __.Zero() = Pure ()

[<AutoOpen>]
module AutoOpen =

    let effect = OpChainBuilder()
