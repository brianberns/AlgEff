namespace AlgEff.Handler

open AlgEff.Effect

/// Generic effect handler.
type EffectHandler<'effect, 'next, 'state, 'finish when 'effect :> Effect<'next>> =
    {
        /// Handler's initial state.
        Start : 'state

        /// Handles a single effect in the given state, answering the updated state
        /// and the next effect.
        Step : ('state * 'effect) -> ('state * 'next)

        /// Transforms the handler's final state.
        Finish : 'state -> 'finish
    }

module EffectHandler =

    /// Creates an effect handler.
    let create start step finish =
        {
            Start = start
            Step = step
            Finish = finish
        }

    /// Maps a function over an effect handler.
    let map f handler =
        create handler.Start handler.Step (handler.Finish >> f)

    /// Runs the given program using the given handler.
    let run program handler =

        /// Runs a single step in the program.
        let rec loop state = function
            | Free effect ->
                let state', next = handler.Step(state, effect)
                loop state' next
            | Pure result ->
                state, result

        let state, result = loop handler.Start program
        result, handler.Finish(state)

    /// Combines two effect handlers.
    let combine
        (handler1 : EffectHandler<'effect1, _, _, _>)
        (handler2 : EffectHandler<'effect2, _, _, _>) =

        let start = handler1.Start, handler2.Start

            // this is the only point at which reflection is necessary
        let step ((state1, state2), (effect : Effect<'next>)) =
            match effect with
                | :? 'effect1 as effect1 ->
                    let state1', next = handler1.Step(state1, effect1)
                    (state1', state2), next
                | :? 'effect2 as effect2 ->
                    let state2', next = handler2.Step(state2, effect2)
                    (state1, state2'), next
                | _ -> failwithf "Unhandled effect: %A" effect

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
type ConcreteContext<'res>() = class end
