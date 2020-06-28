namespace AlgEff.Handler

open AlgEff.Effect

/// Pure log handler.
type PureLogHandler<'ctx, 'res when 'ctx :> LogContext and 'ctx :> ConcreteContext<'res>>(context : 'ctx) =
    inherit EffectHandler<'ctx, 'res, List<string>, List<string>>()

    override __.Start = []

    override __.TryStep(log, effect, cont) =
        match effect with
            | :? LogEffect<EffectChain<'ctx, 'res>> as logEff ->
                let state = logEff.String :: log
                let next = logEff.Cont()
                cont state next |> Some
            | _ -> None

    override __.Finish(log) = List.rev log
