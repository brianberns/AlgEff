namespace AlgEff

type EffectHandler<'state, 'effect, 'next> =
    abstract member Start : 'state
    abstract member Step : 'state * 'effect -> ('state * 'next)
    abstract member Finish : 'state -> 'state

type CombinedEffectHandler<'state1, 'state2, 'effect1, 'effect2, 'next when 'effect1 :> Effect<'next> and 'effect2 :> Effect<'next>>
    (handler1 : EffectHandler<'state1, 'effect1, 'next>,
    handler2 : EffectHandler<'state2, 'effect2, 'next>) =

    interface EffectHandler<'state1 * 'state2, Choice<'effect1, 'effect2>, 'next> with

        member __.Start = handler1.Start, handler2.Start

        member __.Step((state1, state2), effectChoice) =
            match effectChoice with
                | Choice1Of2 effect1 ->
                    let state1', next = handler1.Step(state1, effect1)
                    (state1', state2), next
                | Choice2Of2 effect2 ->
                    let state2', next = handler2.Step(state2, effect2)
                    (state1, state2'), next

        member __.Finish((state1, state2)) =
            handler1.Finish state1, handler2.Finish state2
