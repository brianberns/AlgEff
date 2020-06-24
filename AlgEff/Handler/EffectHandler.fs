namespace AlgEff.Handler

open AlgEff.Effect

/// Generic effect handler.
type EffectHandler<'next, 'state, 'finish> =
    {
        /// Handler's initial state.
        Start : 'state

        /// Attempts to handle a single effect in the given state,
        /// answering the updated state and the next effect if
        /// successful.
        TryStep : ('state * Effect<'next>) -> Option<'state * 'next>

        /// Transforms the handler's final state.
        Finish : 'state -> 'finish
    }

module EffectHandler =

    /// Creates an effect handler.
    let create start step finish =
        {
            Start = start
            TryStep = step
            Finish = finish
        }

    /// Creates an effect handler by adapting the given step function.
    let adapt state step finish =

        let tryStep step (state, effect : Effect<'next>) =
            match effect with
                | :? 'effect as typedEffect ->
                    step (state, typedEffect) |> Some
                | _ -> None

        create state (tryStep step) finish

    /// Maps a function over an effect handler.
    let map f handler =
        create handler.Start handler.TryStep (handler.Finish >> f)

    /// Runs the given program using the given handler.
    let run program handler =

        /// Runs a single step in the program.
        let rec loop state = function
            | Free effect ->
                match handler.TryStep(state, effect) with
                    | Some (state', next) -> loop state' next
                    | None -> failwithf "Unhandled effect: %A" effect
            | Pure result ->
                state, result

        let state, result = loop handler.Start program
        result, handler.Finish(state)

    /// Combines two effect handlers.
    let combine handler1 handler2 =

        let start = handler1.Start, handler2.Start

        let step ((state1, state2), effect) =
            handler1.TryStep(state1, effect)
                |> Option.map (fun (state1', result) ->
                    (state1', state2), result)
                |> Option.orElseWith (fun () ->
                    handler2.TryStep(state2, effect)
                        |> Option.map (fun (state2', result) ->
                            (state1, state2'), result))

        let finish (state1, state2) =
            handler1.Finish state1, handler2.Finish state2

        create start step finish

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
