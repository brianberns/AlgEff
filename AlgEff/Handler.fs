namespace AlgEff

type EffectHandler<'state, 'effect, 'next> =
    {
        Init : 'state
        Step : 'state -> 'effect -> ('state * 'next)
        Final : 'state -> 'state
    }
