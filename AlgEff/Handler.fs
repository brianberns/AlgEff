namespace AlgEff

type EffectHandler<'state, 'effect, 'next when 'effect :> Effect<'next>> =
    {
        Start : 'state
        Step : ('state * 'effect) -> ('state * 'next)
        Finish : 'state -> 'state
    }

module EffectHandler =

    let create start step finish =
        {
            Start = start
            Step = step
            Finish = finish
        }

    let run program handler =

        let rec loop state = function
            | Free effect ->
                let state', next = handler.Step(state, effect)
                loop state' next
            | Pure result ->
                state, result

        let state, result = loop handler.Start program
        result, handler.Finish(state)

    let combine
        (handler1 : EffectHandler<_, 'effect1, _>)
        (handler2 : EffectHandler<_, 'effect2, _>) =

        let start = handler1.Start, handler2.Start

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

    let combine3 handler1 handler2 handler3 =
        combine handler1 handler2
            |> combine handler3
