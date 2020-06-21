namespace AlgEff

[<AbstractClass>]
type EffectHandler<'state, 'effect, 'next when 'effect :> Effect<'next>>() =
    member __.EffectType = typeof<'effect>
    abstract member Start : 'state
    abstract member Step : 'state * 'effect -> ('state * 'next)
    abstract member Finish : 'state -> 'state
    default __.Finish(state) = state

module EffectHandler =

    let run program (handler : EffectHandler<_, _, _>) =

        let rec loop state = function
            | Free effect ->
                let state', next = handler.Step(state, effect)
                loop state' next
            | Pure result ->
                state, result

        let state, result = loop handler.Start program
        result, handler.Finish(state)

type CombinedEffectHandler<'state1, 'state2, 'effect1, 'effect2, 'next when 'effect1 :> Effect<'next> and 'effect2 :> Effect<'next>>
    (handler1 : EffectHandler<'state1, 'effect1, 'next>,
    handler2 : EffectHandler<'state2, 'effect2, 'next>) =
    inherit EffectHandler<'state1 * 'state2, Effect<'next>, 'next>()

    override __.Start = handler1.Start, handler2.Start

    override __.Step((state1, state2), effect) =
        match effect with
            | :? 'effect1 as effect1 ->
                let state1', next = handler1.Step(state1, effect1)
                (state1', state2), next
            | :? 'effect2 as effect2 ->
                let state2', next = handler2.Step(state2, effect2)
                (state1, state2'), next
            | _ -> failwithf "Unhandled effect: %A" effect

    override __.Finish((state1, state2)) =
        handler1.Finish state1, handler2.Finish state2
