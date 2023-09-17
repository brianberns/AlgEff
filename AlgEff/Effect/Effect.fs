namespace AlgEff.Effect

/// An individual effectful operation (e.g. writing to a console).
/// The type parameter is used to chain effects together.
[<AbstractClass>]
type Effect<'a>() =

    /// Maps a function over this effect.
    abstract member Map : ('a -> 'b) -> Effect<'b>

/// A chain of effects (a.k.a. "free monad") that requires a particular
/// context type ('ctx) and returns a particular type ('ret).
type Program<'ctx, 'ret> =

    /// One step in a program.
    | Free of Effect<'ctx, 'ret>

    /// Last step in a program.
    | Pure of 'ret

/// An effect in a program.
and Effect<'ctx, 'ret> = Effect<Program<'ctx, 'ret>>

module Program =

    /// Binds two programs together in the same context.
    let rec bind (f : _ -> Program<'ctx, _>) (program : Program<'ctx, _>) =
        match program with
            | Free effect ->
                effect.Map(bind f) |> Free
            | Pure x -> f x

/// Program builder.
type ProgramBuilder() =
    let (>>=) program f = Program.bind f program
    member _.Bind(program, f) = program >>= f
    member _.Return(value) = Pure value
    member _.ReturnFrom(value) = value
    member _.Zero() = Pure ()
    member _.Combine(program1, program2) =
        program1 >>= (fun () -> program2)
    member _.Delay(f) = f ()

[<AutoOpen>]
module ProgramBuilder =

    /// Program builder.
    let effect = ProgramBuilder()
