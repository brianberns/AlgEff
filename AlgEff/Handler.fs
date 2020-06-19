namespace AlgEff

type EffectHandler<'state, 'effect, 'next when 'effect :> Effect<'next>> =
    abstract member Start : 'state
    abstract member Step : 'state * 'effect -> ('state * 'next)
    abstract member Finish : 'state -> 'state
