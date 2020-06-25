namespace AlgEff.Handler

open AlgEff.Effect

/// Generic effect handler.
[<AbstractClass>]
type EffectHandlerBase<'next, 'state, 'finish>() =

    /// Handler's initial state.
    abstract member Start : 'state

    /// Attempts to handle a single effect in the given state,
    /// answering the updated state and the next effect if
    /// successful.
    abstract member TryStep : 'state * Effect<'next> -> Option<'state * 'next>

    /// Transforms the handler's final state.
    abstract member Finish : 'state -> 'finish

[<AbstractClass>]
type EffectHandler<'ctx, 'res, 'state, 'finish>() =
    inherit EffectHandlerBase<EffectChain<'ctx, 'res>, 'state, 'finish>()

    /// Runs the given program.
    member this.Run(program) =

        /// Runs a single step in the program.
        let rec loop state = function
            | Free (effect : Effect<EffectChain<'ctx, 'res>>) ->
                match this.TryStep(state, effect) with
                    | Some (state', next) -> loop state' next
                    | None -> failwithf "Unhandled effect: %A" effect
            | Pure result ->
                state, result

        let state, result = loop this.Start program
        result, this.Finish(state)

module EffectHandler =

    let create start tryStep finish =
        {
            new EffectHandler<'ctx, 'res, 'state, 'finish>() with
                member __.Start = start
                member __.TryStep(state, effect) = tryStep (state, effect)
                member __.Finish(state) = finish state
        }

    /// Creates an effect handler by adapting the given step function.
    let adapt state step finish =

        let tryStep step (state, effect : Effect<'next>) =
            match effect with
                | :? #Effect<'next> as typedEffect ->
                    step (state, typedEffect) |> Some
                | _ -> None

        create state (tryStep step) finish

    /// Maps a function over an effect handler.
    let map f (handler : EffectHandler<_, _, _, _>) =
        create
            handler.Start
            handler.TryStep
            (handler.Finish >> f)

    /// Combines two effect handlers.
    let combine (handler1 : EffectHandler<'ctx, 'res, 'state1, 'finish1>) (handler2 : EffectHandler<'ctx, 'res, 'state2, 'finish2>) =

        let start = handler1.Start, handler2.Start

        let tryStep ((state1, state2), effect) =
            handler1.TryStep(state1, effect)
                |> Option.map (fun (state1', result) ->
                    (state1', state2), result)
                |> Option.orElseWith (fun () ->
                    handler2.TryStep(state2, effect)
                        |> Option.map (fun (state2', result) ->
                            (state1, state2'), result))

        let finish (state1, state2) =
            handler1.Finish(state1), handler2.Finish(state2)

        create start tryStep finish

    /// Appends a handler.
    let private add a b = combine b a

    /// Combines three effect handlers.
    let combine3 handler1 handler2 handler3 =
        combine handler1 handler2
            |> add handler3
            |> map (fun ((s1, s2), s3) -> s1, s2, s3)

    /// Combines four effect handlers.
    let combine4 handler1 handler2 handler3 handler4 =
        (combine3 handler1 handler2 handler3)
            |> add handler4
            |> map (fun ((s1, s2, s3), s4) -> s1, s2, s3, s4)

/// A concrete context that satisfies an effect's context
/// requirement.
[<AbstractClass>]
type ConcreteContext<'res>() = class end
