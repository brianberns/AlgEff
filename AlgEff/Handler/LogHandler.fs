namespace AlgEff.Handler

open AlgEff.Effect

/// Pure log handler.
type PureLogHandler<'env, 'ret when 'env :> LogContext and 'env :> Environment<'ret>>(env : 'env) =
    inherit SimpleHandler<'env, 'ret, List<string>>()

    /// Start with an empty log.
    override __.Start = []

    /// Adds a string to the log.
    override this.TryStep<'stx>(log, effect, cont) =

        let step log (logEff : LogEffect<_>) cont =
            let log' = logEff.String :: log
            let next = logEff.Cont()
            cont log' next

        this.Adapt<_, 'stx> step log effect cont

    /// Puts the log in chronological order.
    override __.Finish(log) = List.rev log
