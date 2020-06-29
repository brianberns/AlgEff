namespace AlgEff.Handler

open AlgEff.Effect

/// Pure log handler.
type PureLogHandler<'ctx, 'res when 'ctx :> LogContext and 'ctx :> ConcreteContext<'res>>(context : 'ctx) =
    inherit EffectHandler<'ctx, 'res, List<string>, List<string>>()

    override __.Start = []

    override this.TryStep<'outState>(log, effect, cont) =

        let step log (logEff : LogEffect<_>) cont =
            let state = logEff.String :: log
            let next = logEff.Cont()
            cont state next

        this.Adapt<_, 'outState> step log effect cont

    override __.Finish(log) = List.rev log
