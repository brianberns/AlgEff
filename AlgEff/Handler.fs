namespace AlgEff

type EffectHandlerImpl<'state, 'effect, 'next> =
    {
        Init : 'state
        Step : 'state -> 'effect -> ('state * 'next)
        Final : 'state -> 'state
    }

type EffectHandler<'effect> =
    abstract member GetHandler<'state, 'next> : unit -> EffectHandlerImpl<'state, 'effect, 'next>
