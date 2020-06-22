namespace AlgEff.Handler

open AlgEff.Effect

module LogHandler =

    /// Pure log handler.
    let createPure<'ctx, 'res when 'ctx :> LogContext and 'ctx :> ConcreteContext<'res>> (_ : 'ctx) =

        let step (log, (logEff : LogEffect<EffectChain<'ctx, 'res>>)) =
            let state = logEff.String :: log
            let next = logEff.Cont()
            state, next

        EffectHandler.create [] step List.rev
