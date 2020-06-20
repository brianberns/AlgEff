namespace AlgEff

type EffectHandler<'state, 'effect, 'next when 'effect :> Effect<'next>> =
    abstract member Start : 'state
    abstract member Step : 'state * 'effect -> ('state * 'next)
    abstract member Finish : 'state -> 'state

type CombinedEffect<'effect1, 'effect2, 'next when 'effect1 :> Effect<'next> and 'effect2 :> Effect<'next>>(effect1 : 'effect1, effect2 : 'effect2) =
    member __.Effect1 = effect1
    member __.Effect2 = effect2
    interface Effect<'next> with
        member __.Map(f) = CombinedEffect(effect1.Map(f), effect2.Map(f)) :> _

type CombinedEffectHandler<'state1, 'state2, 'effect1, 'effect2, 'next when 'effect1 :> Effect<'next> and 'effect2 :> Effect<'next>>
    (handler1 : EffectHandler<'state1, 'effect1, 'next>,
    handler2 : EffectHandler<'state2, 'effect2, 'next>) =

    interface EffectHandler<'state1 * 'state2, CombinedEffect<'effect1, 'effect2, 'next>, 'next> with

        member __.Start = handler1.Start, handler2.Start

        member __.Step((state1, state2), combinedEffect) =
            let moo = handler1.Step(state1, combinedEffect.Effect1)
            let moo = handler2.Step(state2, combinedEffect.Effect2)

        member __.Finish((state1, state2)) =
            handler1.Finish state1, handler2.Finish state2
